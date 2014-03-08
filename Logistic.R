library(MASS)

## Read Data from C drive ##
a<-read.csv("C:\\lifan\\fl.csv",header=TRUE, sep=",")

## extract data and create numeric variables for chacteristic variables

## Create young, middle-age and old people (1=young, 2=middle-age, 3=old)
age<-as.numeric((2010-a$birthyear)+(11-a$birthmonth)/12+(2-a$birthday)/365)
age_ix <-c()
for(i in 1:length(age)) { age_ix[i]<-1
                        if(age[i] >= 40 & age[i]<=65) age_ix[i]<-2 
                        else if(age[i]>65) age_ix[i]<-3}
age_ix

## Change NA values in vh10g to 0 ##
vh10g <- a$(vh10g)
vh10g[is.na(vh10g)] <- 0

## Create sex numeric code (male =1, female =2) ##
sex <- a$sex
sex_ix <-c()
for(i in 1:length(age)) { sex_ix[i]<-1
                        if(sex[i]=="F") sex_ix[i]<-2}
sex_ix

officialparty<-a$officialparty


party_ix <-c()
for(i in 1:length(age)) { party_ix[i]<-1
                        if(officialparty[i]=="R") party_ix[i]<-2
  if(officialparty[i]=="I") party_ix[i]<-3
  if(officialparty[i]=="E") party_ix[i]<-4
  if(officialparty[i]=="O") party_ix[i]<-5
  if(officialparty[i]=="U") party_ix[i]<-6
}

## Create a new data set
fl <- data.frame(age, age_ix,sex,sex_ix,party_ix,vh10g)
fl.voted <-as.numeric(fl$vh10g >0)

reg.rep<-glm(fl.voted~age+sex_ix+party_ix,family=binomial,data=fl)
summary(reg.rep)
coef(reg.rep)
d.coef <-coef(reg.rep)
d.coef

pre.prob <- fitted(reg.rep)

b<-data.frame(pre.prob)
write.table(b, "C:\\yangh\\lifan\\Lifan_prediction.csv", sep=",")


