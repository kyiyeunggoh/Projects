install.packages('SASxport')
library(SASxport)
library(tidyverse)
library(stringr)
library(ggplot2)
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
library(xgboost)
library(caret)
library(readr)
library(e1071)
library(pscl)
library(lmtest)
library(tidyverse)
library(readxl)
library(pastecs)
library(ranger)
library(rpart)
library(randomForest)

options(scipen=999)


df_cdc=read.xport("LLCP2017.xpt")

df_cdc_1<-df_cdc%>%
  select(state="X.STATE", ever_dep="ADDEPEV2",
         educ_lvl="EDUCA",sex="SEX",employ="EMPLOY1",child="CHILDREN",
         income="X.INCOMG",smoke="SMOKDAY2", bills_pay="SDHBILLS", money_food="SDHFOOD",
         feel_money="SDHMONEY", stress="SDHSTRES", city="MSCODE",race="X.IMPRACE",
         mental_status="X.MENT14D",ment_cont="MENTHLTH",age="X.AGE80", BMI="X.RFBMI5",phy="PA1MIN.")

#select

#recoding levels 
#categorical recoding
df_cdc_1$white<-recode(df_cdc_1$race,"1='1';2='0';3='0';4='0';5='0';6='0'") #0 as not white
df_cdc_1$male<-recode(df_cdc_1$sex,"1='1';2='0';9='NA'") #0 as as female
df_cdc_1$unhealth<-recode(df_cdc_1$BMI,"1='0';2='1';9='NA'") #0 as healthy
df_cdc_1$working<-recode(df_cdc_1$employ,"1='1';2='1';3='0';4='0';5='0';6='1';7='0';8='0';9='NA'") #not working as 0
df_cdc_1$city<-recode(df_cdc_1$city,"1='urban';2='urban';3='suburban';5='rural'")
df_cdc_1$ever_dep<-recode(df_cdc_1$ever_dep,"1='0';2='1'")
#continuous recoding 
df_cdc_1$child[df_cdc_1$child=='88'] <- 0
df_cdc_1$child[df_cdc_1$child=='99'] <- NA
df_cdc_1[,-c(1,6,13,16,17,19)][df_cdc_1[, -c(1,6,13,16,17,19)] > 6] <- NA
df_cdc_1$money_food<-4-df_cdc_1$money_food
df_cdc_1$feel_money<-4-df_cdc_1$feel_money

##predictor as binary: 
df_cdc_1$mental_14<-recode(df_cdc_1$mental_status,"1='0';2='0';3='1';9='NA'") #drop the 9s
df_cdc_1$ment_cont[df_cdc_1$ment_cont>31]<-NA

#Rename states to prep for merging
df_cdc_1$state<-recode(df_cdc_1$state,"1='Alabama';2='Alaska';4='Arizona';
                 5='Arkansas'; 6='California';8='Colorado';9='Connecticut';
                 10='Delaware';11='District of Columbia'; 12='Florida';13= 'Georgia';15='Hawaii';16='Idaho';
                17='Illinois';18='Indiana';19='Iowa';20='Kansas';21='Kentucky';22='Louisiana';
                23='Maine';24='Maryland';25='Massachusetts';26='Michigan';27='Minnesota';28='Mississippi';
                29='Missouri';30='Montana';31='Nebraska';32='Nevada';33='New Hampshire';34='New Jersey';35=
                'New Mexico';36='New York';37='North Carolina';38='North Dakota';39='Ohio';40='Oklahoma';41='Oregon';
                42='Pennsylvania';44='Rhode Island';45='South Carolina';46='South Dakota';47='Tennessee';48='Texas';
                49='Utah';50='Vermont';51='Virgnia';52='Washington';53='Washington';54='West Virginia';55='Wisconsin';
                56='Wyoming';66='Guam';72='Puerto Rico'")

#new clean 
drink<-read_csv("U.S._Chronic_Disease_Indicators__CDI_.csv")

drink_useful <- drink %>% 
  filter(DataValueType=="Crude Prevalence", StratificationCategoryID1=="GENDER"| StratificationCategoryID1== "OVERALL",
         Question=="Poverty"|Question=="Binge drinking prevalence among adults aged >= 18 years")%>%
  select(state="LocationDesc",stateabb="LocationAbbr",year="YearStart",question="Question",Categories="StratificationID1", Data="DataValue")

poverty<-drink_useful%>%
  filter(question=="Poverty",Categories=="OVR")%>%
  select(-question, -Categories)%>%
  rename(poverty=Data)

bingefemale<-drink_useful%>%
  filter(question=="Binge drinking prevalence among adults aged >= 18 years",Categories=="GENF")%>%
  select(-question,-Categories)%>%
  rename(binge_female=Data)

bingemale<-drink_useful%>%
  filter(question=="Binge drinking prevalence among adults aged >= 18 years",Categories=="GENM")%>%
  select(-question,-Categories)%>%
  rename(binge_male=Data)

bingeoverall<-drink_useful%>%
  filter(question=="Binge drinking prevalence among adults aged >= 18 years",Categories=="OVR")%>%
  select(-question,-Categories)%>%
  rename(binge_all=Data)

binge_clean<-left_join(bingeoverall,bingemale)%>%
  left_join(bingefemale)%>%
  full_join(poverty)%>%
  filter(state!="United States")

#I have made a choice to remove additional observations where poverty values are present and keep only 330 observations with all variables present in the binge_clean.csv

write.csv(binge_clean,"binge_clean.csv")


#cleaned data relevant df_cdi variables
drink<-read_csv("U.S._Chronic_Disease_Indicators__CDI_.csv")
binge_clean<-read_csv("binge_clean.csv")
binge_clean_2016<-binge_clean[1:54,]
drink_dirty_2016<-drink[2:55,]
drink_location<-drink_dirty_2016%>%
  select(state="LocationDesc","GeoLocation")
binge_merge<-merge(binge_clean_2016,drink_location,"state")

df_cdc_2<-merge(df_cdc_1, binge_merge, by.x = "state", by.y="state",all=TRUE)

#reduced form model
df_red<-df_cdc_2%>%select("white","male","unhealth","working","child",ever_dep="ever_dep",educlvl="educ_lvl"
                         ,"income","smoke",billspay="bills_pay",moneyfood="money_food",feelmoney="feel_money","stress",
                         "city","race","age",ment14="mental_14","poverty","binge_all")

simple_model<-glm(ment14~as.factor(white)+as.factor(male)+as.factor(unhealth)+as.factor(working)+child+
                    +educlvl+
                    income+smoke+billspay+moneyfood+feelmoney+stress+
                    as.factor(city)+age+binge_all+poverty+as.factor(male)*income+income*age,data=df_red)

reduced_form<-glm(mental_14~log(age)+income+as.factor(working)+stress+income*log(age),data=df_cdc_1)

summary(reduced_form)

summary(simple_model)

varImp(simple_model)

df_red<-na.omit(df_red)

#train and split - actively removed indicator data regarding previous diagnosis 
Train <- createDataPartition(df_red$ment14, p=0.70,list=FALSE)
df_train <- df_red[Train, ]
df_test <- df_red[ -Train, ]

mod_fit<-train(as.factor(ment14)~as.factor(white)+as.factor(male)+as.factor(unhealth)+as.factor(working)+child+
                 +educlvl+
                 income+smoke+billspay+moneyfood+feelmoney+stress+
                 as.factor(city)+age+poverty+binge_all, data=df_train, 
               method="glm", family="binomial")

exp(coef(mod_fit$finalModel))
varImp(mod_fit)
plot(varImp(object=mod_fit),main="RF - Variable Importance")
pred = predict(mod_fit, newdata=df_test)
pR2(simple_model)
confusionMatrix(pred,as.factor(df_test$ment14))
table(pred)
ctable <- as.table(matrix(c(3060, 276, 74, 157), nrow = 2, byrow = TRUE))
fourfoldplot(ctable, color = c("#CC6669", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")

#randomForest
df_red<-na.omit(df_cdc_2%>%
                  select("white","male","unhealth","working","child",educlvl="educ_lvl","income","smoke",billspay="bills_pay",moneyfood="money_food",feelmoney="feel_money","stress",
                         "city","race","age",ment14="mental_14","binge_all","poverty"))
  
#recode all to categorical 
df_red$poverty<-as.factor(df_red$poverty)
df_red$binge_all<-as.factor(df_red$binge_all)
df_red$city<-as.factor(df_red$city)
df_red$male<-as.factor(df_red$male)
df_red$unhealth<-as.factor(df_red$unhealth)
df_red$working<-as.factor(df_red$working)
count(df_red$child)
df_red$child<-recode(df_red$child,"0='none';1='one';2='two';3='three';4='four';
                     5='five';6='six';7='seven';9='nine'")
df_red$child[df_red$child=='99'] <- NA
df_red$child<-as.factor(df_red$child)
df_red$educlvl<-as.factor(df_red$educlvl)
df_red$income<-as.factor(df_red$income)
df_red$smoke<-as.factor(df_red$smoke)
df_red$billspay<-as.factor(df_red$billspay)
df_red$moneyfood<-as.factor(df_red$moneyfood)
df_red$feelmoney<-as.factor(df_red$feelmoney)
df_red$stress<-as.factor(df_red$stress)
df_red$city<-as.factor(df_red$city)
df_ment14<-as.factor(df_red$ment14)
df_red$age<-cut(df_red$age, breaks=c(1,30,51,71,110), 
                   labels=c("geny","genx","babyboom","elderly"))
df_red$age<-as.factor(df_red$age)

df_red1<-df_red%>%
  select(-c("race"))
na.omit(df_red1)
df_red1$ment14<-as.factor(df_red1$ment14)
df_red1$white<-as.factor(df_red1$white)
df_red1$male<-as.factor(df_red1$male)
df_red1$unhealth<-as.factor(df_red1$unhealth)
df_red1$working<-as.factor(df_red1$working)
str(df_red1)

# split into training and testing
set.seed(100)
train_index <- sample(1:nrow(df_red1), 0.8 * nrow(df_red1))
ment_train <- df_red1[train_index, ]
ment_test <- df_red1[-train_index, ]
summary(ment_test)

model11<-randomForest(ment14 ~ ., data = ment_train, ntree = 600, mtry = 4, importance = TRUE)
model11

## preferred because large n, medium m features - tendency to overfit but performs alright
## test data
# Predicting on train set
predValid<- predict(model11, ment_test , type = "class")
mean(predValid == ment_test$ment14)                    
table(predValid,ment_test$ment14)
varImpPlot(model11) 
#stress, previous history of depression, age, billspay, moneyfood

# Decision tree
dt_mod1 = train(ment14 ~ ., data = ment_train, method = "rpart")
dt_mod2 = predict(dt_mod1, data = ment_train)
table(dt_mod2, ment_train$ment14)
mean(dt_mod2 == ment_train$ment14)
summary(dt_mod1)

dt_ver_1 = predict(dt_mod1, newdata = ment_test)
table(dt_ver_1, ment_test$ment14)
mean(dt_ver_1 == ment_test$ment14)

dt_1<-rpart(ment14~., data=ment_train, cp=0.013)
rpart.plot(dt_1, box.palette="RdBu", shadow.col="gray", nn=TRUE)
ctable2 <- as.table(matrix(c(1988, 157, 94, 146), nrow = 2, byrow = TRUE))
fourfoldplot(ctable, color = c("#CC6669", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")

