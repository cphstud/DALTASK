library(dplyr)
library(stringr)
library(rvest)
library(readr)



# https://www.edc.dk/alle-boliger/dannemare/4983/klokkevangen-4/?sagsnr=49205149
alllinks <- read_lines("out2.txt")
tstlinks <- alllinks[1:6]
edcdf <- data.frame(matrix(nrow=1,ncol=6))
edccolnames <- c("description","price","sqmprice","area","casenr","year")
names(edcdf) <- edccolnames
for (link in tstlinks) {
  tpage = read_html(link)
  nfacts = tpage %>% html_nodes("div.case-facts__fact-col:nth-child(1)") %>% html_text()
  nfacts2 = tpage %>% html_nodes("div.case-facts__fact-col:nth-child(2)") %>% html_text()
  nname = tpage %>% html_nodes(".case-facts__fact-col~ .case-facts__fact-col+ .case-facts__fact-col") %>% html_text()
  npris = tpage %>% html_nodes(".col-4.case-facts__header-col") %>% html_text()
  desc = tpage %>% html_nodes(".description p") %>% html_text()
  grpsy <- str_match_all(nfacts2,"([\\d\\/]+)")
  grps <- str_match_all(paste(npris,nname,nfacts),"([\\d\\.]+)")
  resy <- grpsy[[1]]
  res <- grps[[1]]
  sagsnr <- res[19,2]
  year <- resy[2,2]
  price <- res[1,2] 
  sqmprice <- res[7,2]
  desc <- desc[1]
  area <- res[22,2]
  tmpv <- c(desc,price,sqmprice,area,sagsnr,year)
  names(tmpv) <- edccolnames
  edcdf <- rbind(edcdf,tmpv)
}
