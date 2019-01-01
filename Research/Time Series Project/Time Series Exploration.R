---
  title: "Exploration of confidence in religious institutions across time in the United States"
author: "Kyi Yeung Goh"
date: "29/11/2018"
output:
  html_document:
  keep_md: true
---

setwd("~/Desktop/Github Projects/Projects/Research/Time Series Project")
library(corrplot)
library(reshape2)
library(forecast)
library(sandwich)
library(ggplot2)
library(readr)
library(tidyverse)
library(dplyr)
library(zoo)
library(lmtest)
library(car)
library(fUnitRoots)
library(haven)
gss <- read_dta("gss_stata_with_codebook/GSS7216_R4.DTA")
options(scipen=999)
meltMyTS <- function(mv.ts.object, time.var, keep.vars){
  # mv.ts.object = a multivariate ts object
  # keep.vars = character vector with names of variables to keep 
  # time.var = character string naming the time variable
  require(reshape2)
  
  if(missing(keep.vars)) {
    melt.dat <- data.frame(mv.ts.object)
  }
  else {
    if (!(time.var %in% keep.vars)){
      keep.vars <- c(keep.vars, time.var)
    }
    melt.dat <- data.frame(mv.ts.object)[, keep.vars]
  }
  melt.dat <- melt(melt.dat, id.vars = time.var)
  colnames(melt.dat)[which(colnames(melt.dat) == time.var)] <- "time"
  return(melt.dat)
}

ggMyTS <- function(df, varlist, line = TRUE, point = TRUE, pointsize = 3, linewidth = 1.25, ...){
  require(ggplot2)
  # varlist = character vector with names of variables to use
  if(missing(varlist)){
    gg <- ggplot(df, aes(time, value, colour = variable)) 
  }
  else{
    include <- with(df, variable %in% varlist)
    gg <- ggplot(df[include,], aes(time, value, colour = variable))   
  }
  if(line == FALSE & point == FALSE) {
    stop("At least one of 'line' or 'point' must be TRUE") 
  }
  else{
    if(line == TRUE) gg <- gg + geom_line(size = linewidth, aes(color = variable), ...)
    if(point == TRUE) gg <- gg + geom_point(size = pointsize, aes(color = variable), ...)
  }
  
  gg + xlab("") + theme(legend.position = "bottom") + scale_x_continuous(breaks = min(df$time):max(df$time))
} 

firstD <- function(var, group, df){
  bad <- (missing(group) & !missing(df))
  if (bad) stop("if df is specified then group must also be specified")
  
  fD <- function(j){ c(NA, diff(j)) }
  
  var.is.alone <- missing(group) & missing(df)
  
  if (var.is.alone) {
    return(fD(var))
  }
  if (missing(df)){
    V <- var
    G <- group
  }
  else{
    V <- df[, deparse(substitute(var))]
    G <- df[, deparse(substitute(group))]
  }
  
  G <- list(G)
  D.var <- by(V, G, fD)
  unlist(D.var)
}

vars <- c("year", "homosex","conclerg", "sex", "age", "partyid", "marital"
          , "educ", "realinc","race", "pray","attend","size","sexornt","fund")

#Cleaning
sub <- gss[, vars]

#Removing all weird values that dont make sense in codebook
sub[,-c(1,5,8,9,13)][sub[, -c(1,5,8,9,13)] >7] <- NA

#Recoding
sub$partyid<-7-sub$partyid #Strong republican as base
sub$pray<-7-sub$pray #Never as base
sub$conclerg<-4-sub$conclerg #none as base
sub$homosex<-5-sub$homosex #none as base
sub <- mutate(sub, 
              fund=ifelse(fund==1,1,0),
              female=ifelse(sex==2,1,0),
              BA = ifelse(educ >= 13, 1, 0),
              white = ifelse(race == 1,1,0),
              married = ifelse(marital== 5,1,0),
              white= ifelse(race==1,1,0),
              income = realinc)

# get means by year
by.year <- aggregate(subset(sub, sel = -year), list(year = sub$year), mean, na.rm = T)

# interpolate for some missing years
# add the extra years
by.year <- arrange(by.year, year)

# make a time series object by.year.ts and interpolate using na.approx
by.year.ts <- ts(by.year)
by.year.ts <- na.approx(by.year.ts)

by.year.ts <- as.data.frame(by.year.ts)

by.year.ts <- mutate(by.year.ts, 
                     female_pct=female*100,
                     white_pct = white*100,
                     ba_pct = BA*100,
                     married_pct=married*100,
                     fund_pct=fund*100)

# correlations
cor.vars <- c("year", "homosex","conclerg", "female_pct", "age", "partyid", "married_pct"
              , "ba_pct", "realinc","white_pct", "pray","attend","size","sexornt","fund_pct")

cor.dat <- by.year.ts[, cor.vars]

corrplot(cor(cor.dat))

keep.vars <- c("year", "homosex","conclerg", "female_pct", "age", "partyid", "married_pct"
               , "ba_pct", "realinc","white_pct", "pray","attend","size","fund_pct")

plot.dat <- meltMyTS(mv.ts.object = by.year.ts, time.var = "year", keep.vars = keep.vars)

(g_clerg_pct <- ggMyTS(df = plot.dat, varlist = 
                         c("conclerg"))) #clergy trust over time 

(g_age <- ggMyTS(df = plot.dat, varlist = 
                   c("age"))) #Average respondent age over time

(g_degreelt50_pct <- ggMyTS(df = plot.dat, varlist = 
                              c("ba_pct"))) #Average percentage of respondents with at least 1 year of college over time

(g_degreelt50_pct <- ggMyTS(df = plot.dat, varlist = 
                              c("realinc"))) #Average income of respondents over time

(g_degreelt50_pct <- ggMyTS(df = plot.dat, 
                            varlist = c("married_pct"))) #Average percentage of respondents who are married over time

(g_degreelt50_pct <- ggMyTS(df = plot.dat, 
                            varlist = c("fund_pct"))) #Average percentage of respondents who are married over time


lm1<-lm(homosex ~ conclerg + female_pct + age + 
               partyid+married_pct+ba_pct+log(realinc)+white_pct+pray
        +attend+log(size)+fund_pct, data=by.year.ts)

summary(lm1, scientific=FALSE)

bptest(lm1) #no reason to reject null hyppthesis 

# look for autocorrelation in errors
e <- lm1$resid
acf(e) 
acf(e, xlim = c(1,8), col = "red", lwd = 2) # can also customize acf output
plot(e) # plot residuals over time
dwtest(lm1) # Durbin-Watson test
bgtest(lm1) # Breusch-Godfrey test
durbinWatsonTest(lm1, max.lag=2) # Durbin-Watson with more lags

#There is indication that there is autocorrelation(1).

# include year trend
lm2 <- update(lm1, ~ .  + year)
summary(lm2)

# look for autocorrelation
e2 <- lm2$resid
acf(e2, xlim = c(1,8), col = "red", lwd = 2)
pacf(e2, xlim = c(1,8), col = "red", lwd = 2)
plot(e2)
dwtest(lm2)
bgtest(lm2)
#Breusch-Godfrey tests similarly suggests presence of autocorrelation. 
durbinWatsonTest(lm2, max.lag=3)
vif(lm2)
#VIF indicates issues with married pct, ba_pct, year_pct, age, fund_pct, white_pct. 


by.yearFD <- mutate(data.frame(by.year.ts),
                    homosex=firstD(homosex),
                    conclerg = firstD(conclerg),
                    female_pct = firstD(female_pct),
                    age = firstD(age),
                    partyid=firstD(partyid),
                    married_pct=firstD(married_pct),
                    ba_pct=firstD(ba_pct),
                    white_pct=firstD(white_pct),
                    pray = firstD(pray),
                    attend=firstD(attend),
                    size=firstD(size),
                    fund_pct=firstD(fund_pct),
                    year = year)

lm3<-lm(homosex ~ conclerg + female_pct + age + 
              partyid+married_pct+ba_pct+log(realinc)+white_pct+pray
            +attend+size+fund_pct+year, data=by.yearFD)
summary(lm3)
vif(lm3)
e3 <- lm3$resid
acf(e3, xlim = c(1,6), col = "red", lwd = 2)
pacf(e3, xlim = c(1,6), col = "red", lwd = 2)

auto.arima(e3, trace=TRUE)
dwtest(lm3)
bgtest(lm3,order=3)
adfTest(by.yearFD[,"homosex"], lags = 3, type="ct")

summary(lm3)

#Base plots
p <- ggplot(data = by.yearFD, aes(x = year, y = homosex)) + 
  geom_line(color = "#00AFBB", size = 1)

p + stat_smooth(
  color = "#FC4E07", fill = "#FC4E07",
  method = "loess"
)
# There is a clear, unambiguous downwards trend from around 1992 till today in the first-differenced data from the panel.
# The first-differenced percentage of white respondents, church attendance, size of the town and years are significant predictors.


