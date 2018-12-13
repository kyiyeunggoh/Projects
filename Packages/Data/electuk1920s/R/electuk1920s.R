#' 1920s election data
#'
#' This package will allow you to build a database, which is included as an RDS file in the repo, of 1920s election data.
#' This includes the 1922,1923,1924 and 1929 General Elections. This is done by webscraping HTML data from Wikipedia
#' and politicsresources.com. This contains data on the year, candidates, parties, constituencies, swing and margins of each seat.
#' I have also called on the Google Maps API to match the geographical data of the area. As such, it is advisable
#' for the user to obtain an API google maps key before starting.
#'
#' To retrieve this data after building the dataset, the user can obtain the results using the filter
#' function in the tidyverse package by specifying the year and seat of concern.
#'
#' There are two functions in this dataset package:
#' @name polres_scrape
#' @param url Provide a URL link to one of political res's subpages
#' @keywords election, table data
#' @example polres_scrape("http://www.politicsresources.net/area/uk/ge1924/i01.html")
#'
#' @name get_latlong
#' @param area Provide an area name for the function to retrieve data.
#' @keywords longitude, latitude, geographical data
#' @example get_latlong("Columbia University, New York")
#' @export

library(devtools)
library(roxygen2)
library(testthat)
library(rvest)
library(plyr)
library(dplyr)
library(tidyverse)
library(kableExtra)
library(stringr)
library(xml2)
library(httr)
library(data.table)
library(XML)
library(httr)
library(rebus)
library(RJSONIO)
library(googleway)
library(gtools)
library(ggmap)
library(foreach)
library(reshape)


# Creating loop to extract 1922 data
datalist=list()
for (i in 2:261) {
  url<-"https://en.wikipedia.org/wiki/Constituency_election_results_in_the_1922_United_Kingdom_general_election"
  wiki<-read_html(url)
  table<-html_nodes(wiki,css=".wikitable")[i]
  pagename_head<-'//*[@id="mw-content-text"]/div/table/caption'
  pagename_year<-'//*[@id="firstHeading"]'
  table_head<-html_nodes(wiki,xpath=pagename_head)[i]
  table_year<-html_nodes(wiki,xpath=pagename_year)
  results<-(html_table(table, fill=TRUE))[[1]]
  constituency<-as.character(str_extract(html_text(table_head), "[[:alpha:] ]+"))
  year<-as.numeric(str_extract(html_text(table_year), "(\\d)+"))
  results['Constituency']=constituency
  results['Year']=year
  datalist[[i]]<-results
}
election_1922= do.call(rbind, datalist)

# Creating loop to extract 1923 data
datalist1=list()
for (i in 2:588) {
  url<-"https://en.wikipedia.org/wiki/Constituency_election_results_in_the_1923_United_Kingdom_general_election"
  wiki<-read_html(url)
  table<-html_nodes(wiki,css=".wikitable")[i]
  pagename_head<-'//*[@id="mw-content-text"]/div/table/caption'
  pagename_year<-'//*[@id="firstHeading"]'
  table_head<-html_nodes(wiki,xpath=pagename_head)[i]
  table_year<-html_nodes(wiki,xpath=pagename_year)
  results<-(html_table(table, fill=TRUE))[[1]]
  constituency<-as.character(str_extract(html_text(table_head), "[[:alpha:] ]+"))
  year<-as.numeric(str_extract(html_text(table_year), "(\\d)+"))
  results['Constituency']=constituency
  results['Year']=year
  datalist1[[i]]<-results
}
election_1923= do.call(rbind, datalist1)

# Creating loop to extract 1929 data
datalist2=list()
for (i in 2:588) {
  url<-"https://en.wikipedia.org/wiki/Constituency_election_results_in_the_1929_United_Kingdom_general_election"
  wiki<-read_html(url)
  table<-html_nodes(wiki,css=".wikitable")[i]
  pagename_head<-'//*[@id="mw-content-text"]/div/table/caption'
  pagename_year<-'//*[@id="firstHeading"]'
  table_head<-html_nodes(wiki,xpath=pagename_head)[i]
  table_year<-html_nodes(wiki,xpath=pagename_year)
  results<-(html_table(table, fill=TRUE))[[1]]
  constituency<-as.character(str_extract(html_text(table_head), "[[:alpha:] ]+"))
  year<-as.numeric(str_extract(html_text(table_year), "(\\d)+"))
  results['Constituency']=constituency
  results['Year']=year
  datalist2[[i]]<-results
}
election_1929= rbindlist(datalist2, fill=TRUE)
election_1929<-subset(election_1929, select=c(1:8))%>%
  as.data.frame(.)

##NOTE: Function can be written but much faster to extract given that the structure of each page is different
##Hence, manual inspection of the xpaths are needed. I start at 2 because 1 is blank and end at 588 before
##data on university elections which are not relevant for our project. However, data from our second source,
##is far more amenable to extraction via a function and I have created one after testing it on a loop.

# Function to extract based on URL rather than manually keying 14 times of those pages for this website
polres_scrape<-function(url){
  library(tidyverse)
  library(stringr)
  library(rvest)
  listofdata<-list()
  for (i in 1:length(read_html(url)%>%
                     html_nodes(.,css=".inner")%>%
                     as.list(.))) {
    remwiki<-read_html(url)
    tablerem<-html_nodes(remwiki, css=".inner")[i]
    pagename_head<-html_nodes(remwiki,css=".constname")[i]
    table_year<-as.numeric(str_extract(html_text(remwiki,'/html/body/h1'),"(\\d)+"))
    results<-as.data.frame(html_table(tablerem, fill=TRUE))
    constituency<-html_text(pagename_head)
    year<-table_year
    results["Constituency"]=as.character(constituency)
    results['Year']=as.numeric(year)
    results
    listofdata[[i]]<-results
  }
  return(listofdata)
}

d1<-polres_scrape("http://www.politicsresources.net/area/uk/ge1924/i01.html")
d2<-polres_scrape("http://www.politicsresources.net/area/uk/ge1924/i02.html")
d3<-polres_scrape("http://www.politicsresources.net/area/uk/ge1924/i03.html")
d4<-polres_scrape("http://www.politicsresources.net/area/uk/ge1924/i04.html")
d5<-polres_scrape("http://www.politicsresources.net/area/uk/ge1924/i05.html")
d6<-polres_scrape("http://www.politicsresources.net/area/uk/ge1924/i06.html")
d7<-polres_scrape("http://www.politicsresources.net/area/uk/ge1924/i07.html")
d8<-polres_scrape("http://www.politicsresources.net/area/uk/ge1924/i08.html")
d9<-polres_scrape("http://www.politicsresources.net/area/uk/ge1924/i09.html")
d10<-polres_scrape("http://www.politicsresources.net/area/uk/ge1924/i10.html")
d11<-polres_scrape("http://www.politicsresources.net/area/uk/ge1924/i11.html")
d12<-polres_scrape("http://www.politicsresources.net/area/uk/ge1924/i12.html")
d13<-polres_scrape("http://www.politicsresources.net/area/uk/ge1924/i13.html")
d14<-polres_scrape("http://www.politicsresources.net/area/uk/ge1924/i14.html")

elections_1924<-bind_rows(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14)

## At this point in time, I will join the datasets together
election_1924<-elections_1924[c(2,1,3,4,5,6)]
colnames(election_1924)<-c("Party.1","Candidate","Votes","%","Constituency","Year")
election_1922<-election_1922[c(2,3,4,5,7,8)]
election_1923<-election_1923[c(2,3,4,5,7,8)]
election_1929<-election_1929[c(2,3,4,5,7,8)]
election_1920s<-rbind(election_1922,election_1923,election_1924,election_1929)

#Clean to ensure that calls ONLY UK data
distinct_con<-election_1920s%>%
  distinct(Constituency)
distinct_con["Country"]=as.character(", United Kingdom")
distinct_con$Constituency<-str_replace(distinct_con$Constituency, "City", "City of London")
distinct_con$full <- paste(distinct_con$Constituency, distinct_con$Country)

## Extract constituency data and create dataframe for Google API calls using a function
get_latlong <- function(area, apiKey = Sys.getenv("GMAPS_APP_TOKEN")) {
  library(tidyr)
  parameters <- ""
  if (!is.null(apiKey)) {
    parameters <- str_c("&key=", apiKey)
  }
  apiRequests <- iconv(str_c("https://maps.googleapis.com/maps/api/geocode/json?address=", area , parameters), "", "UTF-8")
  result <- c()
  for(i in 1:length(area)) {
    Sys.sleep(0.1)
    conn <- httr::GET(URLencode(apiRequests[i]))
    apiResponse <- jsonlite::fromJSON(httr::content(conn, "text"))
    ac <- apiResponse$results$geometry$location
    if (is.null(ac)) {
      print("NULL: There appears to be problems in your entry.")
    }
    if (apiResponse$status != "OK") {
      warning("status is not OK")
    } else {
      return(ac)
    }
  }
}


#Loop through rows of constituency and return a dataframe for all observations with names
output<-list()
foreach (i = 1:nrow(distinct_con)) %do% {
  result<-get_latlong(distinct_con[i,3])
  output[[i]]<-result
}

constituency_location<-bind_rows(output)

# Join Google API data with electoral data with new 'long/lat' descriptor
full_constituency<-cbind(distinct_con,constituency_location[1:697,])

clean_constituency_1920<-merge(election_1920s,full_constituency,by="Constituency")

election1920sdb<-clean_constituency_1920%>%
  select(Constituency, Party.1, Candidate, Votes, "%", Year, lat, lng)

colnames(election1920sdb)[2]<-"Party"
colnames(election1920sdb)[5]<-"Percentages"
colnames(election1920sdb)[7]<-"Latitude"
colnames(election1920sdb)[8]<-"Longitude"

saveRDS(election1920sdb, file="election1920sdb.rds")

# Some descriptive statistics of the data
#Average vote swings for contested constituencies in 1920s
swingdata<-election1920sdb%>%
  filter(str_detect(Votes,"Swing"))%>%
  filter(str_detect(Percentages,"(?>-)*[[:digit:]]+\\.*[[:digit:]]*"))

swingdata$Percentages<-str_extract(swingdata$Percentages,"[0-9.]+")
mean(as.numeric(swingdata$Percentages))

#There was an average vote swing of 7.100267 for each seat across all
#elections in the 1920s.

#Most common candidate names
namesdata<-election1920sdb %>%
  filter(!str_detect(election1920sdb$Candidate
                     ,"Majority|Turnout|gain|hold|win|Electorate|Conservative|Labour|Liberal|
                     P|politician"))

bagofnames<-str_split(namesdata$Candidate," ")%>%
  unlist(.)%>%
  as.data.frame(.)

freq<-table(bagofnames)
maxFreq<-which.max(freq)
maxFreq

#We have, in the 1920s, 1358 Johns running for election to Parliament

#Longest candidate names
which.max(nchar(namesdata$Candidate))
namesdata[2622,3]

#The longest candidate name is James Archibald St George Fitzwarenne-Despencer Robertson
#who ran for election to the seat of Islington West for the Conservative Party in 1924.

# Most votes in a constituency
rawvotes<-election1920sdb%>%
  filter(!str_detect(Votes,"Swing"))%>%
  filter(str_detect(Votes,"(?>-)*[[:digit:]]+\\.*[[:digit:]]*"))

rawvotes$Votes<-as.numeric(gsub("\\,", "", rawvotes$Votes))

max(as.numeric(rawvotes$Votes))

#The most number of votes won by a candidate was 101 550 votes.

