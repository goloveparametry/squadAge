# TODO
# **Fix the Y axis to be consistent among filtered views
# **Get Average for the whole league


# Load required package(s)

require("ggplot2")
require("XML")


# List of URLs to scrape
# names(url)[1]
# url[[1]]

url <- list(
  "AC Sparta Praha" = "http://www.gambrinusliga.cz/klub/2-ac-sparta-praha.html",
  #"SK Slavia Praha" = "http://www.gambrinusliga.cz/klub//5-sk-slavia-praha.html",
  "FC Viktoria Plzeň" = "http://www.gambrinusliga.cz/klub/6-fc-viktoria-plzen.html")


# List of dates when seasons started

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
  season = numeric(),
  dateBorn = as.Date(character()), 
  position = character(),
  name = character(), 
  team = character(),
  dateSeasonStart = as.Date(character()),
  stringsAsFactors=FALSE
)

# Loop over teams

for(i in url){
  
  # Fetch links (squad by season)
  
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
          tab$`jméno`, # name
          substr(gsub("-","",strsplit(j,"/")[[1]][5]),2,6), # team
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

# Filter option

filter <- list("1" = "No Filter", 
               "2" = "Defense",
               "3" = "Midfield",
               "4" = "Attack")
filterOption <- names(filter)[1]

# Filter by position

dff <-  if(filterOption == "2") df[df$position == "O",] else if(filterOption == "3") dff <- df[df$position == "Z",] else if(filterOption == "4") df[df$position == "U",] else df

# Insufficient data for FCVK prior 2000

if(filterOption != "1") dff <- dff[(as.numeric(levels(dff$season)[dff$season]) > 2000 & dff$team == "fcvik") | (dff$team == "acspa"),]

# Calculate players age at the start of the season

dff$playerAgeYrs <- as.numeric(difftime(dff$dateSeasonStart, dff$dateBorn, units = "days")/365)

# Table aggregated means of squad age in days by club and season
gr <- aggregate(dff$playerAgeYrs, 
                by=list(dff$season, dff$team), 
                mean, 
                na.rm=TRUE)

colnames(gr) <- c(
  "season",
  "team",
  "avgSquadAge"
)

#levels(gr$season) <- rev(levels(gr$season)) # reverse levels
gr$key <- paste(gr$season, gr$team, "")

# Full data frame (in case of missing seasons)
df1 <- expand.grid(season = seq(1994, 2013), team = levels(gr$team))
df1$key <- paste(df1$season, df1$team, "")

# Join full data frame with mean
df1 <- merge(x = df1, y = gr, by = "key", all.x=TRUE)[,c(2,3,6)]
colnames(df1) <- c("season","team","avgSquadAge")

levels(df1$team) <- c(names(url)[1],names(url)[2])

# Visualize

cbPalette <- c("#D55E00","#0072B2") # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

d <- ggplot(df1, aes(x=as.factor(season), y=avgSquadAge, group=team, colour=team )) + 
        geom_point(position="dodge", stat = "identity") +
        geom_line(data=df1[!is.na(df1$avgSquadAge),])


d <- d + scale_colour_manual(values=cbPalette)
d <- d + ylim(20, 30)
d <- d + labs(list(title = paste("Average Squad Age of",  names(url)[1],"and",  names(url)[2], "since 1994 (", if(as.integer(filterOption) <= 4) filter[[filterOption]], ")"), 
                   x = "Season", 
                   y = "Average Squad Age (years)", 
                   colour = "Team" ))
d

# Analyse
# dff[dff$team == "fcvik" & dff$season == 1999,]
# table(dff$position, dff$season, dff$team) filtered data valid since 2000s
# table(dff$season, dff$team)


