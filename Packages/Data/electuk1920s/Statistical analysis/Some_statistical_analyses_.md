---
title: "Some statistics"
author: "Kyi Yeung Goh"
date: "14/12/2018"
output:
  html_document:
    keep_md: true
---



# Some descriptive statistics of the data
## Average vote swings for contested constituencies in 1920s

```r
swingdata<-election1920sdb%>%
  filter(str_detect(Votes,"Swing"))%>%
  filter(str_detect(Percentages,"(?>-)*[[:digit:]]+\\.*[[:digit:]]*"))

swingdata$Percentages<-str_extract(swingdata$Percentages,"[0-9.]+")
mean(as.numeric(swingdata$Percentages))
```

```
## [1] 7.100267
```
There was an average vote swing of 7.100267 for each seat across all elections in the 1920s.

## Most common candidate names

```r
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
```

```
## John 
## 1358
```
We have, in the 1920s, 1358 Johns running for election to Parliament

## Longest candidate names

```r
which.max(nchar(namesdata$Candidate))
```

```
## [1] 2622
```

```r
namesdata[2622,3]
```

```
## [1] "James Archibald St George Fitzwarenne-Despencer Robertson"
```
The longest candidate name is James Archibald St George Fitzwarenne-Despencer Robertson who ran for election to the seat of Islington West for the Conservative Party in 1924.

## Most votes in a constituency

```r
rawvotes<-election1920sdb%>%
  filter(!str_detect(Votes,"Swing"))%>%
  filter(str_detect(Votes,"(?>-)*[[:digit:]]+\\.*[[:digit:]]*"))

rawvotes$Votes<-as.numeric(gsub("\\,", "", rawvotes$Votes))

max(as.numeric(rawvotes$Votes))
```

```
## [1] 101550
```
The most number of votes won by a candidate was 101 550 votes.
