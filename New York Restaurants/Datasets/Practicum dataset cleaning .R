library(reshape2)
library(tidyverse)
library(data.table)
library(readxl)
library(stringr)
library(rebus)
library(RJSONIO)
library(googleway)
library(httr)
library(ggmap)
library(seplyr)
library(car)
library(plyr)
library(dplyr)
library(fuzzyjoin)


inspect<-read.csv(file.choose())
rats<-read.csv(file.choose())
zipcode<-read_xlsx(file.choose())
restcomball<-read.csv(file.choose())
comballdata<-read.csv(file.choose())

#Recoding violation code into categorical violations
inspect$violationtype<-recode_factor(inspect$VIOLATION.CODE,'02A'='foodhandle','02B'='foodhandle','02C'='foodhandle','02D'='foodhandle',
'02E'='foodhandle','02F'='foodhandle','02G'='foodhandle','02H'='foodhandle','02I'='foodhandle',
'02J'='foodhandle','03A'='foodhandle','03B'='foodhandle','03C'='foodhandle','03D'='foodhandle',
'03E'='foodhandle','03F'='foodhandle','03G'='foodhandle','04A'='foodhandle','04B'='foodhandle',
'04C'='foodhandle', '04D'='foodhandle','04E'='foodhandle','04F'='foodhandle','04G'='foodhandle',
'04H'='foodhandle','04I'='foodhandle','04J'='clean',"04K"='clean', "04L"='clean',"04M"='clean',
"04N"='clean',"04O"='clean',"05A"='clean',"05B"='clean',"05C"='clean',"05D"='clean',"05E"='clean',
"05F"='clean',"05G"='clean',"05H"='clean',"05I"='clean','06A'='workers','06B'='workers','06C'='foodhandle'
,'06C'='foodhandle','06D'='foodhandle','06E'='foodhandle','06F'='foodhandle','06H'='foodhandle','06I'='foodhandle'
,'07A'='misc','08A'='safety','08B'='clean','08C'='foodhandle','09A'='foodhandle','09B'='foodhandle'
,'09C'='foodhandle','10A'='foodhandle','10B'='clean','10C'='safety','10D'='foodhandle','10E'='foodhandle',
'10F'='foodhandle','10G'='foodhandle','10H'='misc','10I'='foodhandle','10J'='foodhandle','99B'='misc',
'15A'='misc','15B'='misc','15C'='misc','15D'='misc','15E'='misc','15F'='misc','15G'='misc','15H'='misc','15I'='misc'
,'15J'='misc','15K'='misc','15L'='misc','15M'='misc','15N'='misc', '15O'='misc','15S'='misc','15T'='misc', '16A'='misc',
'16B'='misc','16C'='misc','16D'='misc','16E'='misc','16F'='misc','18A'='safety','18B'='safety','18C'='safety',
'18D'='safety','18E'='safety','18F'='misc','18G'='misc','18H'='workers','20A'='misc','20B'='misc','20C'='misc',
'20D'='misc','20E'='misc','20F'='safety','22A'='safety','22B'='misc','22C'='misc','22D'='misc','22E'='misc',
'22F'='misc','06G'='misc','22G'='misc','17A'='misc','19A'='misc')

#Recoding critical value is binomial
inspect$criticalscore<-recode_factor(inspect$CRITICAL.FLAG, 'Critical'= '1', 'Not Critical'='0')

#Filtering only necessary stuff
inspection<-inspect%>%
select(CAMIS, DBA, CUISINE.DESCRIPTION, SCORE, GRADE, criticalscore, violationtype,ZIPCODE, ACTION, BUILDING, STREET)%>%
group_by(ZIPCODE)

na.omit(inspection)

#Drawing closed and not closed using stringr as outcome variable
inspection$ACTION<-str_detect(inspection$ACTION, "[Clc]losed")

inspection$ACTION<-as.integer(as.logical(inspection$ACTION))

#Nearby incidents - creating known rat issues in ZIP -> transforming to rates per sq km
ratszipcount<-as.data.frame(table(rats$ZIP_CODE))%>%
select(ZIPCODE="Var1", COUNT="Freq")

zipcode[] <- lapply(zipcode, factor)

combratdata<-left_join(ratszipcount,zipcode)

combratdata2<-na.omit(combratdata)

combratdata2$`Land Area` <- as.numeric(combratdata2$`Land Area`)

combratdata2<-combratdata2%>%
mutate(Rate=COUNT/`Land Area`)

comballdata<-left_join(inspection, combratdata2)

na.omit(comballdata)

comballdata$DBA<-gsub("([^A-Za-z0-9 ])+", "", x = comballdata$DBA)

write.csv(comballdata, "opendataclean.csv")

#Scraping for restaurant prices and matching them using Google API - prices and reviews
#Use Places API to obtain Price and Rating
#Expand to all observations

APIset<-distinct(comballdata, DBA, .keep_all=TRUE)%>%
unite(., "address",c("BUILDING","STREET"), sep = ' ')%>%
unite(.,"searchstr",c("DBA", "address"), sep=" ")

APIset$searchstr<-gsub("([^A-Za-z0-9 ])+", "", x = APIset$searchstr)

restinfo <- apply(APIset, 1, function(x){
    google_places(search_string = x[['searchstr']],
    place_type="restaurant",
    simplify = TRUE,
    key = "AIzaSyA_0NzSeJOJLW2rygAfNffSMuXaKUTNdoI" )
})

restaurantAPIcalls<-restinfo

str(restinfo)


coords <- lapply(seq_along(res), function(x){
    library(tidyr)
    coords <- res[[x]]$results$geometry$location
    address <- as.data.frame(res[[x]]$results$address_components[[1]])%>%
    select_se("long_name")%>%
    t(.)%>%
    paste(.,collapse=" ")
    res_df <- data.frame(lat = coords[, 'lat'],
    lon = coords[, 'lng'],
    address = address
    )
})

#target for that row - name, location/latlong, price and rating columns -> join using searchstr -> copy and expand into bigger one -> remove NAs
restkeyattr_name<-lapply(seq_along(restinfo), function(x){
    name<-restinfo[[x]]$results$name
})

restnamesdf<-as.data.frame(unlist(restkeyattr_name))

restkeyattr_coords<-lapply(seq_along(restinfo), function(x){
    coords<-restinfo[[x]]$results$formatted_address
})

restcoordsdf<-as.data.frame(unlist(restkeyattr_coords))

restkeyattr_location<-lapply(seq_along(restinfo), function(x){
    location<-restinfo[[x]]$results$geometry$location
})

restlocationdf<-as.data.frame(unlist(restkeyattr_location))


restkeyattr_rating<-lapply(seq_along(restinfo), function(x){
    rating<-restinfo[[x]]$results$rating
})

restratingdf<-as.data.frame(unlist(restkeyattr_rating))

write.csv(restpricelvldf, "restpricelvldf.csv")

#For price level return NA if it does not exist
restkeyattr_pricelvl<- sapply(seq_along(restinfo), function(x){
    pricelvl<-restinfo[[x]]$results$price_level
})

restpricelvldf<-as.data.frame(unlist(restkeyattr_pricelvl))

#combine all
comballdata<-comballdata%>%
unite(., "Address",c("BUILDING","STREET"), sep = ' ')

restcomball<-cbind(restnamesdf,restcoordsdf,restratingdf)

restcomball<-restcomball%>%
select(DBA="unlist(restkeyattr_name)",Address="unlist(restkeyattr_coords)", Rating="unlist(restkeyattr_rating)")

restcomball$Name<-gsub("([^A-Za-z0-9])+", "", x = restcomball$Name)

restcomball = as.data.frame(sapply(restcomball, toupper))

restcomball$DBA<-as.factor(restcomball$DBA)

restcomball$Address<-gsub("(.*),.*", "\\1", restcomball$Address)%>%
gsub("(.*),.*", "\\1", .)%>%
gsub("(.*),.*", "\\1", .)%>%
gsub("([^A-Za-z0-9 ])+", "",.)

restcomball$Address<-str_replace(restcomball$Address, "STR","STREET")%>%
str_replace(.,"AVE","AVENUE")%>%
str_replace(.,"RD","ROAD")%>%
str_replace(.,"BLVD","BOULEVARD")

fullset<-merge(comballdata, restcomball, by.x = "Address", by.y = "Address", all.x = TRUE)

fullset<-na.omit(fullset)

fullset<-fullset%>%
filter(str_detect(Address,"USA"))%>%
filter(str_detect(Address,"NY"))

fullset$Rating<-as.numeric(as.character(fullset$Rating))

fullset$Rate<-as.numeric(as.character(fullset$Rate))

fullset$SCORE<-as.numeric(as.character(fullset$SCORE))

uniqfull<-fullset%>%
distinct(CAMIS,.keep_all=TRUE)


#Split 70-30 set
train<-fullset[]

## 75% of the sample size
smp_size <- floor(0.75 * nrow(mtcars))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(mtcars)), size = smp_size)

train <- mtcars[train_ind, ]
test <- mtcars[-train_ind, ]

#Running regressions - CLEAN IT UP!
comballdata$GRADE<-as.factor(comballdata$GRADE)
comballdata$violationtype<-as.factor(comballdata$violationtype)
comballdata$criticalscore<-as.factor(comballdata$criticalscore)

model1<-glm(ACTION~SCORE+GRADE+violationtype+log(Rate)
+criticalscore, family=binomial, data=comballdata)

summary(model1)

#CARRY OUT ROBUSTNESS CHECKS!!!!!!!


#Spare code

res <- apply(APIset, 1, function(x){
    google_geocode(address = x[['address']],
    key = key)
})

res<-APIgeo

res[[3]]$results$address_components[[1]]%>%
select("long_name")%>%
t(.)%>%
paste(.,collapse=" ")


#-----------------

coords <- lapply(seq_along(res), function(x){
    library(tidyr)
    coords <- res[[x]]$results$geometry$location
    address <- as.data.frame(res[[x]]$results$address_components[[1]])%>%
    select_se("long_name")%>%
    t(.)%>%
    paste(.,collapse=" ")
    res_df <- data.frame(lat = coords[, 'lat'],
    lon = coords[, 'lng'],
    address = address
    )
})

df_coords <- do.call(rbind, coords)
df_coords%>%
as.data.frame(.)

install.packages("ddply")
install.packages("plotly")
library(ggplot2)
library(ddply)
library(plotly)
ggplot(model1, aes(x=SCORE, y=ACTION)) + geom_point() +
stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+
geom_vline(xintercept=13)

ggplot(a, aes(ACTION))+
geom_bar(aes(fill=GRADE), position="fill")

a<-comballdata%>%
select(criticalscore,ACTION,GRADE)%>%
filter(!GRADE=="Not Yet Graded")%>%
arrange(ACTION)

a[a==""]<-NA

a<-a[complete.cases(a),]
