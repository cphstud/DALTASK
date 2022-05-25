library(dplyr)
library(stringr)
library(rvest)
library(readr)
library(mongolite)


con <-mongo(
  collection = "villaer",
  db = "edc",
  url = "mongodb://localhost",
  verbose = TRUE
)

con$find(limit=5)

# https://www.edc.dk/alle-boliger/dannemare/4983/klokkevangen-4/?sagsnr=49205149
alllinks <- read_lines("out2.txt")
tstlinks <- alllinks[5:20]
allpages <- list()

edcdf <- data.frame(matrix(nrow=1,ncol=6))
edccolnames <- c("description","price","sqmprice","area","_id","year")
names(edcdf) <- edccolnames

for (i in 1:length(tstlinks)) {
  tpage <-  read_html(tstlinks[[i]])
  Sys.sleep(as.integer(abs(10*rnorm(1))))
  allpages[[i]] <- tpage
}

testallpages <- allpages

for (xpage in allpages) {
  print(class(xpage))
}

for (xpage in allpages) {
  #print("GO")
  nfacts = xpage %>% html_nodes("div.case-facts__fact-col:nth-child(1)") %>% html_text()
  nfacts2 = xpage %>% html_nodes("div.case-facts__fact-col:nth-child(2)") %>% html_text()
  nname = xpage %>% html_nodes(".case-facts__fact-col~ .case-facts__fact-col+ .case-facts__fact-col") %>% html_text()
  npris = xpage %>% html_nodes(".col-4.case-facts__header-col") %>% html_text()
  desc = xpage %>% html_nodes(".description p") %>% html_text()
  grpsy <- str_match_all(nfacts2,"([\\d\\/]+)")
  grps <- str_match_all(paste(npris,nname,nfacts),"([\\d\\.]+)")
  resy <- grpsy[[1]]
  res <- grps[[1]]
  #sagsnr <- getsagsnr
  sagsnr <- getsagsnr(nfacts)
  #year <- resy[2,2]
  year <- getyear(nfacts2)
  #price <- res[1,2] 
  price <- getprice(npris)
  #sqmprice <- res[7,2]
  sqmprice <- getsqm(nname)
  #desc <- desc[1]
  desc <- getdesc(desc)
  #area <- res[22,2]
  area <- getarea(nfacts)
  tmpv <- c(desc,price,sqmprice,area,sagsnr,year)
  names(tmpv) <- edccolnames
  if (sagsnr != "NA") {
  edcdf <- rbind(edcdf,tmpv)
  } else {
  print("NO SAGSNR .. skipping line")
  }
}

con$remove(query="{}")
con$insert(edcdf)

getdesc = function(x){
  res <- tryCatch(res <- x[1], error = function(e) { return(NA)} )
  return(res)
}


getprice = function(x){
  res <- tryCatch({
    res <- str_match_all(x,"([\\d\\.]{5,})")
    ym <- res[[1]]
    res <- ym[1,2]} ,
    error = function(e){ return(NA)}
  )
  return(res)
}

getsagsnr = function(x){
  res <- tryCatch({
    res <- str_match_all(x,"([0-9]{7,})")
    ym <- res[[1]]
    res <- ym[1,2]} ,
    error = function(e){ return(NA)}
  )
  return(res)
}

getsqm = function(x){
  res <- tryCatch({
    res <- str_match_all(x,"([0-9\\.]{4,}) ")
    ym <- res[[1]]
    res <- ym[1,2]} ,
    error = function(e){ return(NA)}
  )
  return(res)
}

getarea = function(x){
  res <- tryCatch({
    res <- str_match_all(x,"([0-9]+) ")
    ym <- res[[1]]
    res <- ym[1,2]} ,
    error = function(e){ return(NA)}
  )
  return(res)
}

getyear = function(x){
  res <- tryCatch({
    res <- str_match_all(x,"([0-9]{4})")
    ym <- res[[1]]
    res <- ym[1,2]} ,
    error = function(e){ return(NA)}
  )
  return(res)
}

