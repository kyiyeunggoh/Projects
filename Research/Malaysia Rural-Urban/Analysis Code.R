library(readr)
library(readxl)
library(tidyverse)
library(corrplot)
library(reshape2)
library(forecast)
library(sandwich)
library(ggplot2)
library(readr)
library(tidyverse)
library(zoo)
library(lmtest)
library(car)
library(fUnitRoots)
library(plm)
library(plyr)
library(stargazer)
library(dplyr)
library(caret)
library(rms)
library(estimatr)
library(broom)
library(e1071)
library(gvlma)
library(visreg)
library(pastecs)
library(kableExtra)
options(scipen=999)

wvs<-readRDS("WVS6")
wvs_short<-wvs%>%
  select("income"=V239,
         "townsize"=V253,
         "educ_lvl"=V248,
         "import_pol"=V84,
         "conf_gov"=V115,
         "nat_pride"=I_NATIONALISM,
         "pol_pol"=V95,
         "trust"=V24,
         "country"=V2,
         "ethnic"=V254,
         "relig"=V146,
         "con_econ"=V60,
         "corr_acp"=V202,
         "dem_gov"=V141,
         "age"=V242)

wvs_ms<-wvs_short%>%
  dplyr::filter(country=="458")

#recoding
wvs_ms$conf_gov<-5-wvs_ms$conf_gov
wvs_ms$relig<-9-wvs_ms$relig
wvs_ms$con_econ<-ifelse(wvs_ms$con_econ == 1, 1, 0)
wvs_ms$townsizecat<-recode(wvs_ms$townsize,"1='0';2='0';3='0'
                          ;4='0';5='1';6='1';7='1';8='1'")
wvs_ms$areatype<-recode(wvs_ms$townsize,"1='Rural';2='Rural';3='Rural'
                             ;4='Suburban';5='Suburban';6='Urban';7='Urban';8='Urban'")
wvs_ms$agecat<-cut(wvs_ms$age, breaks=c(16,30,51,71,110), 
                      labels=c("geny","genx","babyboom","elderly"))
wvs_ms$educ_lvlrecode<-recode(wvs_ms$educ_lvl,"1=1;2=1;3=1
                                 ;4=2;5=2;6=2;7=2;8=3;9=3")
#Removing all negative values
wvs_ms[,-c(9,10,18)][wvs_ms[, -c(9,10,18)] < 0] <- NA

#Descriptive statistics
res <- stat.desc(wvs_ms[,-c(9,10,18,17)])
round(res, 2)
colSkewness(wvs_ms$townsizecat, na.rm=TRUE)


#Simple models
simple_model<-lm(conf_gov~townsize,data=wvs_ms)
simple_model1<-lm(conf_gov~townsizecat,data=wvs_ms)
simple_model2<-lm(conf_gov~areatype,data=wvs_ms)


stargazer(simple_model, simple_model1, simple_model2,
          title="Regression Results", 
          intercept.top = TRUE,
          intercept.bottom = FALSE,
          align=TRUE, 
          dep.var.labels=c("Confidence in Government"), 
          column.labels=c("Model1.1", "Model 1.2", "Model 1.3"), 
          ci=TRUE,
          type = "text",
          out="html")

lm1_4<-lm(conf_gov~income+townsize+educ_lvl
          +corr_acp+relig+dem_gov+pol_pol+age+as.factor(ethnic)
          +nat_pride,data=wvs_ms)

lm1_5<-lm(conf_gov~income+townsize+educ_lvl
          +corr_acp+relig+dem_gov+pol_pol+age+as.factor(ethnic)
          +nat_pride+trust+import_pol,data=wvs_ms)

lm1_6<-lm(conf_gov~income+areatype+educ_lvl
          +corr_acp+relig+dem_gov+pol_pol+age+as.factor(ethnic)
          +nat_pride+trust+import_pol,data=wvs_ms)

lm1_7<-lm(conf_gov~income+townsizecat+educ_lvl
          +corr_acp+relig+dem_gov+pol_pol+age+as.factor(ethnic)
          +nat_pride+trust+import_pol,data=wvs_ms)

stargazer(lm1_4, lm1_5, lm1_6,lm1_7,
          title="Regression Results", 
          intercept.top = TRUE,
          intercept.bottom = FALSE,
          align=TRUE, 
          dep.var.labels=c("Confidence in Government"), 
          column.labels=c("1.4", "1.5", "1.6", "1.7"), 
          ci=TRUE,
          type = "text",
          out="html")

varImp(lm1_5)
varImp(lm1_7)

#Removing all negative values
wvs_ms[,-c(9,10,18,19)][wvs_ms[, -c(9,10,18,19)] < 0] <- NA

summary(wvs_ms$conf_gov)

#Modelling

#Check interaction effects
lm2<-lm(conf_gov~income+townsize+educ_lvlrecode+corr_acp+dem_gov+pol_pol+age+as.factor(ethnic)+nat_pride
        +trust+import_pol+townsize*import_pol,data=wvs_ms)

summary(lm2)


lm2_3lvl<-lm(conf_gov~income+townsizecat+educ_lvlrecode+
                      dem_gov+pol_pol+age+as.factor(ethnic)+nat_pride
                    +trust+import_pol+townsizecat*import_pol,data=wvs_ms)

summary(lm2_3lvl)


lm2_areatype<-lm(conf_gov~income+areatype+educ_lvlrecode+corr_acp+
                   dem_gov+pol_pol+age+as.factor(ethnic)+nat_pride
                 +trust+import_pol+areatype*import_pol,data=wvs_ms)
summary(lm2_areatype)

stargazer(lm2, lm2_3lvl, lm2_areatype,
          title="Regression Results", 
          intercept.top = TRUE,
          intercept.bottom = FALSE,
          align=TRUE, 
          dep.var.labels=c("Confidence in Government"), 
          column.labels=c("1.8", "1.9", "1.10"), 
          ci=TRUE,
          type = "text",
          out="html")

#Other statistical tests
vif(lm2_3lvl) #collinearity present
bptest(lm2_3lvl) #no homoscedasticity

#two things: (i) mean-centering and (ii) box-cox transformation

#NICEL Assumptions of model
par(mfrow=c(2,2))
lm2_3lvlres=rstandard(lm2_3lvl)
plot(lm2_3lvlres) #linearity and independence 
hist(lm2_3lvlres) #normality histogram of std.res
plot(lm2_3lvl)

#Address VIF - mean-center our observations 
mean(wvs_ms$townsizecat,na.rm=T)
mean(wvs_ms$import_pol,na.rm=T)
wvs_ms$center.towncat <- scale(wvs_ms$townsizecat, center = T, scale = F)
wvs_ms$center.import_pol <- scale(wvs_ms$import_pol, center = T, scale = F)
summary(wvs_ms$center.towncat)
summary(wvs_ms$center.import_pol)

lm2c<-lm(conf_gov~income+townsizecat+educ_lvlrecode+nat_pride+
           con_econ+dem_gov+trust+pol_pol+center.import_pol+agecat+as.factor(ethnic)+
           townsizecat*center.import_pol,data=wvs_ms)

#Other statistical tests
vif(lm2c) #collinearity present
bptest(lm2c) #have homoscedasticity


#Box-Cox Transforamtion  (just for the heck of it)
confgovBCMod<-BoxCoxTrans(as.numeric(wvs_ms$conf_gov))
print(confgovBCMod)
wvs_ms<-cbind(wvs_ms,conf_gov_new=predict(confgovBCMod,as.numeric(wvs_ms$conf_gov)))
lm2C_BC<-lm(conf_gov_new~income+townsizecat+educ_lvlrecode+nat_pride+
           con_econ+dem_gov+trust+pol_pol+center.import_pol+agecat+as.factor(ethnic)+
           townsizecat*center.import_pol,data=wvs_ms)
summary(lm2C_BC)

stargazer(lm2C_BC,
          title="Regression Results", 
          align=TRUE, 
          dep.var.labels=c("Confidence in Government"), 
          no.space=TRUE, 
          column.labels=c("Final Model with B-C transformation"),
          dep.var.caption="Conf_gov", 
          model.numbers=FALSE,
          type = "text")

par(mfrow=c(2,2))
lm2_3centresBC=rstandard(lm2C_BC)
plot(lm2_3centresBC) #linearity and independence 
hist(lm2_3centresBC) #normality histogram of std.res
plot(lm2C_BC)

bptest(lm2C_BC)


# Visualisation of interaction effects
visreg(lm(conf_gov_new ~ as.numeric(center.import_pol)*townsizecat, data = wvs_ms),
       "center.import_pol", by = "townsizecat" , overlay=T, band = F, partial = F,
       line = list(col = c("dodgerblue" ,"orangered" )), bty = "l", legend = F)

legend("bottomleft" , c("Rural", "Urban"), lwd = 2, col = c("dodgerblue", "orangered"),
       cex = 0.8)





hatvalues(lm2C_BC)


vif
bptest
vif(lmTHREEcent)
gvlma(x = lmTHREEcent)
bptest(lmTHREEcent)
car::ncvTest(lmTHREEcent)


plot(lmTHREEcent) #linearity and independence 
hist(lm2_3lvlres) #normality histogram of std.res
predict.plot(lm2_3lvl)
#NICEL Assumptions of model
par(mfrow=c(2,2))
lm2_3centres=rstandard(lmTHREEcent)
plot(lm2_3centres) #linearity and independence 
hist(lm2_3centres) #normality histogram of std.res
plot(lmTHREEcent)
predict.plot(lm2_3lvl)
#Box-Cox Transforamtion  (just for the heck of it)
confgovBCMod<-BoxCoxTrans(as.numeric(wvs_ms$conf_gov))
print(confgovBCMod)
wvs_ms<-cbind(wvs_ms,conf_gov_new=predict(confgovBCMod,as.numeric(wvs_ms$conf_gov)))
lmTHREEcentBC<-lm(conf_gov_new~income+center.townsz3+educ_lvlrecode+nat_pride+
                    con_econ+corr_acp+dem_gov+trust+pol_pol+center.import_pol+agecat+as.factor(ethnic)+
                    center.townsz3*center.import_pol,data=wvs_ms)
gvlma(x = lmTHREEcentBC)
bptest(lmTHREEcentBC)
car::ncvTest(lmTHREEcentBC)

stargazer(lmTHREEcentBC,
          title="Regression Results", 
          align=TRUE, 
          dep.var.labels=c("Confidence in Government"), 
          no.space=TRUE, 
          column.labels=c("Final Model with B-C transformation"),
          dep.var.caption="Conf_gov", 
          model.numbers=FALSE,
          type = "text", omit = "Constant")

par(mfrow=c(2,2))
lm2_3centresBC=rstandard(lmTHREEcentBC)
plot(lm2_3centresBC) #linearity and independence 
hist(lm2_3centresBC) #normality histogram of std.res
plot(lmTHREEcentBC)
predict.plot(lm2_3lvl)




