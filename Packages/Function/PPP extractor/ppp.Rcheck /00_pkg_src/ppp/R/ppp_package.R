#' PPP extractor function
#'
#' Check a country's PPP in a given year.
#'
#' This function helps you solve trawling through Google for crucial information on PPP.
#'
#' The function asks the user to fill in the country followed by the year of desired observation
#' should this not be an error it will return a tibble with information relevant to the query.
#' @name findppp
#' @param country Provide a country code found in OECD's R package
#' @param year Provide the year that you wish to extract data for
#' @keywords PPP, economic data, year
#' @examples To retrieve PPP data for a particular year, just specify the country then year
#' econdata("AUS","2001")

library(OECD)
library(dplyr)

PPPGDP<-get_dataset("PPPGDP")

df<-PPPGDP%>%
  select("LOCATION","UNIT","obsTime","obsValue")%>%
  filter(UNIT=="NATUSD")%>%
  select(country="LOCATION",year="obsTime",PPP="obsValue")

na.omit(df)

econdata<- function(x,y){
  df%>%
    filter(country==x)%>%
    filter(year==y)
}

