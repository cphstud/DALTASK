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

onelink <-  "https://www.edc.dk/alle-boliger/aabenraa/6200/klinkbjerg-5/?sagsnr=61104296"
# start with one
one.tpage <- read_html(onelink)
ftag <- ".case-facts__fact-col:nth-child(1) .case-facts__two-col:nth-child(1) .info"
ftag2 <- ".case-facts__fact-col:nth-child(1) .case-facts__two-col:nth-child(1) .value"
ftag3 <- ".case-facts__fact-col:nth-child(2) .case-facts__two-col:nth-child(2) .value"
sectag <- "div.case-facts__fact-col"
#first.section <- html_children()
num <- one.tpage %>% html_nodes(ftag2) %>% html_text()
num2 <- one.tpage %>% html_nodes(ftag3) %>% html_text()
num3 <- one.tpage %>% html_nodes(sectag) %>% html_text()
secs <- one.tpage %>% html_nodes(sectag)
mylist <- list()

for (i in 1:length(secs)) {
    key = secs[[i]] %>% html_nodes(".info") %>% html_text
    #print(c("G: ",html_text(s)))
    print(c(i," K ",key))
 
}
for (s in secs) {
 print("FIRST ...") 
  keys = s %>% html_nodes(".info")
  vals = s %>% html_nodes(".value")
  for (i in 1:length(keys)) {
    text = html_text(keys[[i]])
    val = html_text(vals[[i]])
    print(c("GOT",i, " ",text," -> ",val))
    mylist[[ text ]] <- val
    
  }
}
for (s in secs) {
 print("FIRST ...") 
  keys = s %>% html_nodes(".info")
  vals = s %>% html_nodes(".value")
  for (k in 1:length(keys)) {
    text = html_text(keys[[i]])
    val = html_text(vals[[i]])
    print(c("GOT",text," -> ",val))
  }
}



# then more
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

