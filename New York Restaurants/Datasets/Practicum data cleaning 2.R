library(caret)
library(readr)
library(e1071)
library(pscl)
library(lmtest)
library(tidyverse)
library(readxl)
library(pastecs)

zipdata<-read_xlsx("ZIPLandArea.xlsx")
restcomball <- read_csv("restcomball.csv")
opendataclean <- read_csv("opendataclean.csv")
fullsetselect<- read_csv("combcleaned.csv")
fullsetselectplus<-read_rds("combcleanedwithZIP.rds")
original<-read_csv("nycrestinspect.csv")

#Cleaning API Calls to match general data form
restcomball<-restcomball%>%
  filter(str_detect(Address,"USA"))%>%
  filter(str_detect(Address,"NY"))

restcomball$Name<-gsub("([^A-Za-z0-9])+", " ", x = restcomball$Name)

restcomball$Address<-gsub("(.*),.*", "\\1", restcomball$Address)%>%
  gsub("(.*),.*", "\\1", .)%>%
  gsub("(.*),.*", "\\1", .)%>%
  gsub("([^A-Za-z0-9 ])+", "",.)

restcomball = as.data.frame(sapply(restcomball, toupper))

restcomball$Address<-str_replace(restcomball$Address, "STR","STREET")%>%
  str_replace(.,"AVE","AVENUE")%>%
  str_replace(.,"RD","ROAD")%>%
  str_replace(.,"BLVD","BOULEVARD")

fullset<-merge(opendataclean, restcomball, by.x = "Address", by.y = "Address", all.x = TRUE)

fullset<-fullset%>%
  select(-GRADE)

fullset$Rating<-as.numeric(as.character(fullset$Rating))
fullset$Rate<-as.numeric(as.character(fullset$Rate))
fullset$SCORE<-as.numeric(as.character(fullset$SCORE))
fullset$violationtype<-as.factor(fullset$violationtype)
fullset$criticalscore<-as.factor(fullset$criticalscore)

fullsetselect<-fullset%>%
  select(Name, Address, ACTION, SCORE, violationtype,Rate, criticalscore, Rating, ZIPCODE)

fullsetselect<-na.omit(fullsetselect)

fullsetselect<-fullsetselect%>%
  filter(!(criticalscore=="Not Applicable"))

model1<-glm(ACTION~SCORE+as.factor(violationtype)+log(Rate)
            +as.factor(criticalscore)+Rating, family=binomial, data=fullsetselect)

summary(model1)

write_rds(fullsetselectplus, "combcleanedwithZIP.rds")
write_rds(zipdata,"nyczipdata.rds")

zipdata<-na.omit(zipdata)
zipdata<-zipdata%>%
  filter(!(hhincome=="NA"))

#Merge with new data on median HH income in ZIP code 
fullsetselectplus<-merge(fullsetselect,zipdata, by.x="ZIPCODE",by.y="ZIPCODE",all.x = TRUE)
fullsetselectplus<-fullsetselectplus%>%
  filter(!(criticalscore=="Not Applicable"))
fullsetselectplus$ACTION<-as.factor(fullsetselectplus$ACTION)
fullsetselectplus$violationtype<-as.factor(fullsetselectplus$violationtype)
fullsetselectplus$criticalscore<-as.factor(fullsetselectplus$criticalscore)
fullsetselectplus$hhincome<-as.numeric(fullsetselectplus$hhincome)
fullsetselectplus$SCORE<-as.numeric(fullsetselectplus$SCORE)
fullsetselectplus<-na.omit(fullsetselectplus)


## Training and cleaning 
Train <- createDataPartition(fullsetselectplus$ACTION, p=0.65, list=FALSE)
training <- fullsetselectplus[Train, ]
testing <- fullsetselectplus[ -Train, ]

mod_fit<-train(ACTION ~ SCORE+as.factor(violationtype)+Rating+as.factor(criticalscore)+ log(hhincome) + log(Rate), data=training, 
            method="glm", family="binomial")

exp(coef(mod_fit$finalModel))

varImp(mod_fit)

plot(varImp(object=mod_fit),main="RF - Variable Importance")

pred = predict(mod_fit, newdata=testing)

mod_fit_one <- glm(ACTION~SCORE+as.factor(violationtype)+log(Rate)
                   +as.factor(criticalscore)+Rating+log(hhincome), data=training, family="binomial")
summary(mod_fit_one)
pR2(mod_fit_one)

#Confusion matrix
confusionMatrix(pred,testing$ACTION)
table(pred)
ctable <- as.table(matrix(c(54315, 1260, 283, 506), nrow = 2, byrow = TRUE))
fourfoldplot(ctable, color = c("#CC6669", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")

#Visuals
preds <- predict(mod_fit_two, newdata = testing, type = 'response',se = TRUE)
testing$pred.full <- preds$fit

testing$ymin <- testing$pred.full - 2*preds$se.fit
testing$ymax <- testing$pred.full + 2*preds$se.fit  

ggplot(testing,aes(x = SCORE , y = as.factor(ACTION))) + 
  facet_wrap(~violationtype) + 
  geom_point() + 
  geom_ribbon(data = testing,aes(y = pred.full, ymin = ymin, ymax = ymax),alpha = 0.25) +
  geom_line(data = testing,aes(y = pred.full),colour = "blue")


## Looking at nth and (n-1) inspection
original_reinspect<-original %>%
  filter(str_detect(`INSPECTION TYPE`, "Re-inspection"))%>%
  arrange(CAMIS)

original_reinspect$previnspect <- c(NA, original_reinspect$`INSPECTION DATE`[-nrow(original_reinspect)])
original_reinspect$prevstatus<- c(NA, original_reinspect$ACTION[-nrow(original_reinspect)])
original_reinspect$prevscore<-c(NA, original_reinspect$SCORE[-nrow(original_reinspect)])

original_reinspect_closed<-original_reinspect%>%
  filter(str_detect(ACTION, "C|closed"))


original_reinspect_closed$prevstatus<-str_detect(original_reinspect_closed$prevstatus, "[Clc]losed")
original_reinspect_closed$prevstatus<-as.numeric(original_reinspect_closed$prevstatus)
sum(original_reinspect_closed$prevstatus==0)
original_reinspect_closed_prevopen<-original_reinspect_closed%>%
  filter(prevstatus=="0")
original_reinspect_closed_prevopen<-original_reinspect_closed_prevopen%>%
  mutate(scorediff=SCORE-prevscore)
stat.desc(original_reinspect_closed_prevopen$prevscore)
stat.desc(original_reinspect_closed_prevopen$SCORE)

a<-original_reinspect_closed_prevopen %>%
  group_by(`ZIPCODE`, sort=TRUE) %>% 
  count(.)

original_reinspect_closed_prevclosed<-original_reinspect_closed%>%
  filter(prevstatus=="1")
original_reinspect_closed_prevclosed<-original_reinspect_closed_prevclosed%>%
  mutate(scorediff=SCORE-prevscore)
stat.desc(original_reinspect_closed_prevclosed$prevscore)
stat.desc(original_reinspect_closed_prevclosed$SCORE)

a<-original_reinspect_closed_prevclosed %>%
  group_by(BORO, sort=TRUE) %>% 
  count(.)
