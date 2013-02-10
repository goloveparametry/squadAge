# club page URLs
url <- c("http://www.gambrinusliga.cz/klub//2-ac-sparta-praha.html","http://www.gambrinusliga.cz/klub//6-fc-viktoria-plzen.html")

# reference dates


for(i in url){
  
  # club
  print(i)
  
  # get links to all seasons
  s <- xpathSApply(htmlParse(i),"//select/option", function(x) c(xmlAttrs(x)[["value"]]))[1:20]
  
  # loop over the links
  for(j in s) {
    
    print(j)
    # get squad table for given season
    surl1 <- paste("http://www.gambrinusliga.cz", j, sep = "")
    # parsing the tree
    doc <- htmlParse(surl1, encoding = "UTF-8")
    # extracting tables from html
    assign(paste(substr(gsub("-","",strsplit(j,"/")[[1]][5]),2,6),strsplit(j,"/")[[1]][3], sep=""), readHTMLTable(doc, stringsAsFactors = FALSE)[[6]])
  }
  
  # format table
  
  
}





# write to file
# write.table(table,"s1.txt")