# club page URLs
url <- c("http://www.gambrinusliga.cz/klub//2-ac-sparta-praha.html","http://www.gambrinusliga.cz/klub//6-fc-viktoria-plzen.html")

df <- data.frame(a = factor(), b = factor(), c = factor(), d = character(), e = factor(), f = factor())

# reference dates - start of the season
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

for(i in url){
  
  # club
  print(i)
  
  # get links to all seasons NEEDS FIXING!!!
  s <- xpathSApply(htmlParse(i),"//select/option", function(x) c(xmlAttrs(x)[["value"]]))
  s <- s[grepl("/soupiska/",s)]
  
  # loop over the links
  for(j in s) {
    
    # data location
    print(j)
    
    # get squad table for given season
    surl1 <- paste("http://www.gambrinusliga.cz", j, sep = "")
    
    # parsing the tree
    doc <- htmlParse(surl1, encoding = "UTF-8")
    
    # extracting tables from html
    tab <- readHTMLTable(doc, stringsAsFactors = FALSE)[[6]]
    
    # formating
    tab$narozen <- as.Date(tab$narozen,"%d.%m.%Y")
    tab$seasonStart <- as.Date(rd[[strsplit(j,"/")[[1]][3]]],"%Y-%m-%d")
    
    df <- rbind(df, data.frame(strsplit(j,"/")[[1]][3], 
                               tab$narozen, 
                               tab$P, 
                               tab$`jméno`, 
                               substr(gsub("-","",strsplit(j,"/")[[1]][5]),2,6),
                               tab$seasonStart
                  )
    )
    
    # store as variable
    # assign(paste(substr(gsub("-","",strsplit(j,"/")[[1]][5]),2,6),strsplit(j,"/")[[1]][3], sep=""), tab)
    # break
  }
  
  
}


colnames(df) <- c("seasonId","dateBorn","position","name","club","seasonStart")


# analysis
# table(df$year,df$club)
gr <- aggregate(df$seasonStart-df$dateBorn, by=list(df$seasonId, df$club), mean, na.rm=TRUE)

# plot
d <- ggplot(gr, aes(x=Group.1, y=as.integer(x), group=Group.2, colour=Group.2 )) + 
        geom_bar(position="dodge", stat = "identity")

# write to file
