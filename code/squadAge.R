# TODO
# **Get Average for the whole league
# **Add counts to data frame gr

# Load required package(s)

require("ggplot2")
require("XML")


# List of dates when seasons started (hardcoded)

rd <- c(
  "2013" = "2012-07-27",
  "2012" = "2011-07-29",
  "2011" = "2010-07-16",
  "2010" = "2009-07-25",
  "2009" = "2008-08-03",
  "2008" = "2007-08-06",
  "2007" = "2006-07-30",
  "2006" = "2005-08-06",
  "2005" = "2004-08-08",
  "2004" = "2003-07-26",
  "2003" = "2002-07-24",
  "2002" = "2001-07-28",
  "2001" = "2000-07-29",
  "2000" = "1999-07-31",
  "1999" = "1998-08-03",
  "1998" = "1997-08-02",
  "1997" = "1996-08-12",
  "1996" = "1995-07-31",
  "1995" = "1994-08-08",
  "1994" = "1993-08-15"
)

# Empty dataframe

df <- data.frame(  
  season = numeric(), # year of the season start (yyyy)
  dateBorn = as.Date(character()), # date player was born
  position = character(), # position of the player
  name = character(), # name of the player
  team = character(), # team the player plays for
  dateSeasonStart = as.Date(character()), # date of the season start
  stringsAsFactors = F
)

## FETCH

# Fetch URLs for every team in current season (named vector)
url <- xpathSApply(
  htmlParse("http://www.gambrinusliga.cz/"),
  "/html/body/header/div/div[2]/div[1]/div[1]/ul/li/a", 
  function(x) c(paste("http://www.gambrinusliga.cz", xmlAttrs(x)[["href"]], sep = ""))
  )

names(url) <- xpathSApply(
  htmlParse("http://www.gambrinusliga.cz/"),
  "/html/body/header/div/div[2]/div[1]/div[1]/ul/li/a/img", 
  function(x) c(xmlAttrs(x)[["title"]])
)

# Loop over teams

for(i in url){
  
  # Fetch links (squad by season)
  print(i)
  
  s <- xpathSApply(htmlParse(i),"//select/option", function(x) c(xmlAttrs(x)[["value"]]))
  s <- s[grepl("/soupiska/",s)]
  
  # Loop over season
  
  for(j in s) {
    
    # Console Log
    print(paste("Fetching: ", j))
    
    # Create URL for team and season
    surl1 <- paste("http://www.gambrinusliga.cz", j, sep = "")
    
    # Fetch html content from URL
    doc <- htmlParse(surl1, encoding = "UTF-8")
    
    # Extract table with squad detail
    tab <- readHTMLTable(doc, stringsAsFactors = FALSE)[[6]] # 6th table is squad details
    
    # Bind data to empty data frame
    df <- rbind(df, data.frame(
          strsplit(j,"/")[[1]][3], # season
          as.Date(tab$narozen,"%d.%m.%Y"), # reformat to yyyy-mm-dd,
          tab$P, # position
          tab[,3], # name
          #substr(gsub("-","",strsplit(j,"/")[[1]][5]),2,6), # team
          names(url[url == i]),
          as.Date(rd[[strsplit(j,"/")[[1]][3]]],"%Y-%m-%d")  # find date with season start from rd
        )
    )
    
  }
  
}

# Rename columns of df

names(df) <- c(
  "season",
  "dateBorn",
  "position",
  "playerName",
  "team",
  "dateSeasonStart"  
  )

# Calculate players age at the start of the season

df$playerAgeYrs <- as.numeric(difftime(df$dateSeasonStart, df$dateBorn, units = "days")/365)

# Total average

avg <- aggregate(dff$playerAgeYrs, by=list(dff$season), FUN = mean, na.rm = T)
avg$team <- "total"
names(avg) <- c("season", "team", "avgSquadAge")
avg <- avg[ , c(1, 3, 2) ]

# FILTER

filter <- list("1" = "No Filter", 
               "2" = "Defense",
               "3" = "Midfield",
               "4" = "Attack")
filterOption <- names(filter)[1]

# Filter by position (dff: data frame filtered)

dff <-  if(filterOption == "2") df[df$position == "O",] else if(filterOption == "3") dff <- df[df$position == "Z",] else if(filterOption == "4") df[df$position == "U",] else df

# Filter by team

# AC Sparta Praha and FC Viktoria Plzen

dff <-  dff[dff$team == "club-acsppr" | dff$team == "club-fcvipl", ]


## AGGREGATE

# Table aggregated means of squad age in days by club and season
gr <- aggregate(dff$playerAgeYrs, 
                by = list(dff$season, dff$team), 
                mean, 
                na.rm = T)

colnames(gr) <- c(
  "season",
  "team",
  "avgSquadAge"
)

#levels(gr$season) <- rev(levels(gr$season)) # reverse levels
gr$key <- paste(gr$season, gr$team, "")

## REFINE

# Full data frame (in case of missing seasons)
df1 <- expand.grid(season = seq(1994, 2013), team = levels(gr$team))
df1$key <- paste(df1$season, df1$team, "")

# Join full data frame with mean
df1 <- merge(x = df1, y = gr, by = "key", all.x = T)[, c(2, 3, 6)]
colnames(df1) <- c("season", "team", "avgSquadAge")

df1 <- rbind(df1, avg)


# VISUALIZE

cbPalette <- c("#D55E00","#0072B2", "#999999") # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

d <- ggplot(df1[!is.na(df1$avgSquadAge), ], aes(x=as.factor(season), y=avgSquadAge, group=team, colour=team )) + 
        geom_point(position="dodge", stat = "identity") +
        geom_line(data=df1[!is.na(df1$avgSquadAge),])


d <- d + scale_colour_manual(values=cbPalette)
d <- d + ylim(20, 30)
d <- d + labs(list(title = paste("Average Squad Age of", names(url)[1], "and", names(url)[2], "since 1994 (", if(as.integer(filterOption) <= 4) filter[[filterOption]], ")"), 
                   x = "Season", 
                   y = "Average Squad Age (years)", 
                   colour = "Team" ))
d

# Analyse
# dff[dff$team == "fcvik" & dff$season == 1999,]
# table(dff$position, dff$season, dff$team) filtered data valid since 2000s
# table(dff$season, dff$team)


