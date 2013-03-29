# TODO
# DONE **Get Average for the whole league
<<<<<<< HEAD
# **Add counts to data frame df.agr
=======
# DONE **Add counts to data frame df.agr
>>>>>>> Averages for whole league added and display of a mean filtered by min 3 occurences

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

# Fetch URL
url <- xpathSApply(
  htmlParse("http://www.gambrinusliga.cz/index.php"),
  "/html/body/header/div/div[2]/div[1]/div[1]/ul/li/a", 
  function(x) c(paste("http://www.gambrinusliga.cz", xmlAttrs(x)[["href"]], sep = ""))
  )

# Fetch team name
names(url) <- xpathSApply(
  htmlParse("http://www.gambrinusliga.cz/index.php", encoding="UTF-8"),
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

# Calculate players age at the start of the given season

df$playerAgeYrs <- as.numeric(difftime(df$dateSeasonStart, df$dateBorn, units = "days")/365)

# FILTER

# Position filter
filter <- list("1" = "No Filter", 
               "2" = "Defense",
               "3" = "Midfield",
               "4" = "Attack")

# Set position filter HERE 
filterOption <- names(filter)[1]

df.f <-  if(filterOption == "2") df[df$position == "O",] else if(filterOption == "3") dff <- df[df$position == "Z",] else if(filterOption == "4") df[df$position == "U",] else df

# Filter by team

# # AC Sparta Praha and FC Viktoria Plzen
# [1] "club-fkslov" "club-fkprib" "club-acsppr" "club-fcbaos" "club-fchrkr" "club-fkslli"
# [7] "club-fcvipl" "club-fcvyji" "club-fczbbr" "club-fkbaja" "club-duklap" "club-fkmlbo"
# [13] "club-fktepl" "club-skdyce" "club-sksiol" "club-skslpr"
df.f <-  df.f[df.f$team == levels(df.f$team)[3] | df.f$team == levels(df.f$team)[7], ]
df.f$team <- factor(df.f$team)

# Total average

df.all <- aggregate(df.f$playerAgeYrs, by=list(df.f$season), FUN = mean, na.rm = T)
df.all$team <- "All"
df.all <- df.all[ , c(1, 3, 2) ]
names(df.all) <- c("season", "team", "avgSquadAge")
df.all$freq <- as.data.frame(table(df$season))[, 2]


## AGGREGATE

# Table aggregated means of squad age in days by club and season
df.agr <- aggregate(df.f$playerAgeYrs, 
                by = list(df.f$season, df.f$team), 
                mean, 
                na.rm = T)

colnames(df.agr) <- c(
  "season",
  "team",
  "avgSquadAge"
)

df.agr$key <- paste(df.agr$season, df.agr$team, "")

# Merge with player counts
df.freq <- as.data.frame(table(df.f$team, df.f$season))
colnames(df.freq) <- c(
  "team",
  "season",
  "freq"
)
df.freq$key <- paste(df.freq$season, df.freq$team, "")
df.agr <- merge(x = df.freq, y = df.agr, by = "key", all.y = T)[, c(1,2,3,4,7)]

colnames(df.agr) <- c(
  "key",
  "team",
  "season",
  "freq",
  "avgSquadAge"
)

## REFINE

# Full data frame (in case of missing seasons)
df.1 <- expand.grid(season = seq(1994, 2013), team = levels(df.agr$team))
df.1$key <- paste(df.1$season, df.1$team, "")

# Join full data frame with mean
df.1 <- merge(x = df.1, y = df.agr, by = "key", all.x = T)[, c(2, 3, 6, 7)]
colnames(df.1) <- c("season", "team", "freq", "avgSquadAge")

# Add rows with totals
df.1 <- rbind(df.1, df.all)


# VISUALIZE

cbPalette <- c("#D55E00", "#0072B2", "#999999") # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

d <- ggplot(data = df.1[!is.na(df.1$avgSquadAge) & df.1$freq > 2, ], aes(x = as.factor(season), y = avgSquadAge, group = team, colour = team)) + 
  geom_point(position = "dodge", stat = "identity") +
  geom_line()


d <- d + scale_colour_manual(values = cbPalette)
d <- d + ylim(20, 30)
d <- d + labs(list(title = paste("Average Squad Age of", df.1$team[1], "and", df.1$team[2], "since 1994 (", if(as.integer(filterOption) <= 4) filter[[filterOption]], ")"), 
                   x = "Season", 
                   y = "Average Squad Age (years)", 
                   colour = "Team" ))
d

# Analyse
# dff[dff$team == "fcvik" & dff$season == 1999,]
# table(dff$position, dff$season, dff$team) filtered data valid since 2000s
# table(dff$season, dff$team)

