library(caret)
library(readr)
library(e1071)
library(pscl)
library(lmtest)

restcomball <- read_csv("restcomball.csv")
opendataclean <- read_csv("opendataclean.csv")
fullsetselect<- read_csv("combcleaned.csv")

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
  select(Name, Address, ACTION, SCORE, violationtype,Rate, criticalscore, Rating)

fullsetselect<-na.omit(fullsetselect)

fullsetselect<-fullsetselect%>%
  filter(!(criticalscore=="Not Applicable"))

model1<-glm(ACTION~SCORE+as.factor(violationtype)+log(Rate)
            +as.factor(criticalscore)+Rating, family=binomial, data=fullsetselect)

summary(model1)

write_csv(fullsetselect, "combcleaned.csv")


fullsetselect$ACTION<-as.factor(fullsetselect$ACTION)
fullsetselect$violationtype<-as.factor(fullsetselect$violationtype)
fullsetselect$criticalscore<-as.factor(fullsetselect$criticalscore)

## Training and cleaning 
Train <- createDataPartition(fullsetselect$ACTION, p=0.65, list=FALSE)
training <- fullsetselect[ Train, ]
testing <- fullsetselect[ -Train, ]

mod_fit<-train(ACTION~SCORE+violationtype+log(Rate)
            +criticalscore+Rating, data=training, 
            method="glm",family="binomial")

mod_fit_two<-train(as.factor(ACTION)~SCORE+as.factor(violationtype)+log(Rate)
                 +Rating, data=training, method="glm",family="binomial")

exp(coef(mod_fit$finalModel))

varImp(mod_fit)

plot(varImp(object=mod_fit),main="RF - Variable Importance")

predictions=predict.train(object=mod_fit,testing[,"ACTION"],type="prob")

mod_fit_one <- glm(ACTION~SCORE+as.factor(violationtype)+log(Rate)
                   +as.factor(criticalscore)+Rating, data=training, family="binomial")

mod_fit_two<-glm(ACTION~SCORE+as.factor(violationtype)+log(Rate)
                 +Rating, data=training, family="binomial")

anova(mod_fit_two, mod_fit_one, test ="Chisq") #drop critical score

pR2(mod_fit_two)

pred = predict(mod_fit, newdata=testing)
pred2 = predict(mod_fit_two,newdata = testing)

#Confusion matrix
confusionMatrix(pred,testing$ACTION)
table(pred)

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

