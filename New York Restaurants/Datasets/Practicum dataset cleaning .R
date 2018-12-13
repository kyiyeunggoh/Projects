inspect<-read.csv(file.choose())
rats<-read.csv(file.choose())
pubbin<-read.csv(file.choose())
zipcode<-read_xlsx(file.choose())

install.packages("reshape2")
library(reshape2)
library(tidyverse)
library(data.table)

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
  select(CAMIS, DBA, CUISINE.DESCRIPTION, SCORE, GRADE, criticalscore, violationtype,ZIPCODE)%>%
  group_by(ZIPCODE)

na.omit(inspection)

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

#Running regressions 
model1<-glm(criticalscore~SCORE+as.factor(GRADE)+as.factor(violationtype)+log(Rate), family=binomial, data=comballdata)

summary(model1)




