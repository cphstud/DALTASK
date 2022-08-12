library(dplyr)
library(stringr)
library(rvest)
library(readr)
library(mongolite)
library(rjson)


con <-mongo(
  collection = "edc13",
  db = "edc",
  url = "mongodb://localhost",
  verbose = TRUE
)

alllinks <- read_lines("out2.txt")
tstlinks <- alllinks[901:2000]

for (i in 1:length(tstlinks)) {
  print(c("doing ...",tstlinks[[i]]))
  tryCatch(doOnePage(tstlinks[[i]]),
           error = function(e)
             print(c("Error: ",e)))
  Sys.sleep(as.integer(abs(4*rnorm(1))))
}
print(c("DONE ...",i))

doOnePage = function(link) {
  mylist <- list()
  one.tpage <- read_html(link)
  ziptag <- ".zip-city"
  sectag <- "div.case-facts__fact-col"
  zip <- one.tpage %>% html_nodes(ziptag) %>% html_text()
  secs <- one.tpage %>% html_nodes(sectag)
  
  mylist[['zip']] <- zip
  mylist[['scrapedate']] <- as.character(Sys.Date())
  
  for (s in secs) {
   print("FIRST ...") 
    keys = s %>% html_nodes(".info")
    vals = s %>% html_nodes(".value")
    for (i in 1:length(keys)) {
      key = html_text(keys[[i]])
      key = str_remove(key, "[æøå]")
      val = html_text(vals[[i]], trim = T)
      mylist[[ key ]] <- val
    }
  }
  myedcdf <- as.data.frame(mylist)
  names(myedcdf)
  names(myedcdf)[names(myedcdf) == "Sagsnr."] <- "_id"
  #myedcdf$Prisudvikling <- gsub("(%).*","\\1",myedcdf$Prisudvikling)
  con$insert(myedcdf)
}

cleanup = function(mcon) {
  mcon$remove(query="{}")
}
