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
vh10g <- a$vh10g
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

reg.rep<-glm(fl.voted~age+sex_ix+party_ix,family=binomial,data=fl.rep)
summary(reg.rep)
coef(reg.rep)
d.coef <-coef(reg.rep)
d.coef




## General analysis using all data
print("Chi-Square Test (All Data)")
chisq.test(fl$sex,fl$vh10g)
chisq.test(fl$age_ix,fl$vh10g)
chisq.test(fl$officialparty,fl$vh10g)

## analysis by party - Democrate and Republican
## Democrate  ##
print("Chi-Square Test (Democrate Only)")
fl.demo <-subset(fl,subset=officialparty=="D")
chisq.test(fl.demo$sex_ix,fl.demo$vh10g)
chisq.test(fl.demo$age_ix,fl.demo$vh10g)


## Futher subset data - voted vs not voted ##
## Democrate voted at polls##
print("Chi-Square Test by Voter Type (Democrate Only)")
fl.demo.voted <-as.numeric(fl.demo$vh10g >0 & fl.demo$vh10g <2)
chisq.test(fl.demo$sex_ix,fl.demo.voted)
chisq.test(fl.demo$age_ix,fl.demo.voted)

## Democrate absentee vote##
fl.demo.abs <-as.numeric(fl.demo$vh10g >1 & fl.demo$vh10g <3)
chisq.test(fl.demo$sex_ix,fl.demo.abs)
chisq.test(fl.demo$age_ix,fl.demo.abs)

## Democrate early vote##
fl.demo.early <-as.numeric(fl.demo$vh10g >6 & fl.demo$vh10g <8)
chisq.test(fl.demo$sex_ix,fl.demo.early)
chisq.test(fl.demo$age_ix,fl.demo.early)

print("Logistic Regression by Voter Type (Democrate Only)")
reg.demo<-glm(fl.demo.voted~age_ix+sex_ix,family=binomial,data=fl.demo)
summary(reg.demo)

d.coef <-coef(reg.demo)

p <- matrix(c(0,0,0,0,0,0), byrow=T, nrow=3)
for(i in 1:3) 
  for (j in 1:2) 
p[i,j]<-exp(d.coef[1]+d.coef[2]*i+d.coef[3]*j)/(1+exp(d.coef[1]+d.coef[2]*i+d.coef[3]*j))

print("Probability of Casting Vote at the Polls")
rownames(p)<-c("Young","Middle-Aged","Old")
colnames(p) <-c("Male","Female")
p



reg.demo<-glm(fl.demo.abs~age_ix+sex_ix,family=binomial,data=fl.demo)
summary(reg.demo)
d.coef <-coef(reg.demo)

p <- matrix(c(0,0,0,0,0,0), byrow=T, nrow=3)
for(i in 1:3) 
  for (j in 1:2) 
p[i,j]<-exp(d.coef[1]+d.coef[2]*i+d.coef[3]*j)/(1+exp(d.coef[1]+d.coef[2]*i+d.coef[3]*j))

print("Probability of Casting Vote at the Polls")
rownames(p)<-c("Young","Middle-Aged","Old")
colnames(p) <-c("Male","Female")
p

reg.demo<-glm(fl.demo.early~age_ix+sex_ix,family=binomial,data=fl.demo)
summary(reg.demo)

coef(reg.demo)
d.coef <-coef(reg.demo)

p <- matrix(c(0,0,0,0,0,0), byrow=T, nrow=3)
for(i in 1:3) 
  for (j in 1:2) 
p[i,j]<-exp(d.coef[1]+d.coef[2]*i+d.coef[3]*j)/(1+exp(d.coef[1]+d.coef[2]*i+d.coef[3]*j))

print("Probability of Casting Vote at the Polls")
rownames(p)<-c("Young","Middle-Aged","Old")
colnames(p) <-c("Male","Female")
p




##Republican ##
print("Chi-Square Test (Republican Only)")
fl.rep <-subset(fl,subset=officialparty=="R")
chisq.test(fl.rep$sex_ix,fl.rep$vh10g)
chisq.test(fl.rep$age_ix,fl.rep$vh10g)

## Futher subset data - voted vs not voted ##
## Republican voted at polls##
print("Chi-Square Test by Voter Type (Republican Only)")
fl.rep.voted <-as.numeric(fl.rep$vh10g >0 & fl.rep$vh10g <2)
chisq.test(fl.rep$sex_ix,fl.rep.voted)
chisq.test(fl.rep$age_ix,fl.rep.voted)



## Republican absentee vote##
fl.rep.abs <-as.numeric(fl.rep$vh10g >1 & fl.rep$vh10g <3)
chisq.test(fl.rep$sex_ix,fl.rep.abs)
chisq.test(fl.rep$age_ix,fl.rep.abs)

## Republican early vote##
fl.rep.early <-as.numeric(fl.rep$vh10g >6 & fl.rep$vh10g <8)
chisq.test(fl.rep$sex_ix,fl.rep.early)
chisq.test(fl.rep$age_ix,fl.rep.early)



### Logistic Regression
print("Logistic Regression by Voter Type (Republican Only)")
reg.rep<-glm(fl.rep.voted~age_ix+sex_ix,family=binomial,data=fl.rep)
summary(reg.rep)
coef(reg.rep)
d.coef <-coef(reg.rep)

p <- matrix(c(0,0,0,0,0,0), byrow=T, nrow=3)
for(i in 1:3) 
  for (j in 1:2) 
p[i,j]<-exp(d.coef[1]+d.coef[2]*i+d.coef[3]*j)/(1+exp(d.coef[1]+d.coef[2]*i+d.coef[3]*j))

print("Probability of Casting Vote at the Polls")
rownames(p)<-c("Young","Middle-Aged","Old")
colnames(p) <-c("Male","Female")
p


reg.rep<-glm(fl.rep.abs~age_ix+sex_ix,family=binomial,data=fl.rep)
summary(reg.rep)
d.coef <-coef(reg.rep)

p <- matrix(c(0,0,0,0,0,0), byrow=T, nrow=3)
for(i in 1:3) 
  for (j in 1:2) 
p[i,j]<-exp(d.coef[1]+d.coef[2]*i+d.coef[3]*j)/(1+exp(d.coef[1]+d.coef[2]*i+d.coef[3]*j))

print("Probability of Casting Vote at the Polls")
rownames(p)<-c("Young","Middle-Aged","Old")
colnames(p) <-c("Male","Female")
p



reg.rep<-glm(fl.rep.early~age_ix+sex_ix,family=binomial,data=fl.rep)
summary(reg.rep)

d.coef <-coef(reg.rep)

p <- matrix(c(0,0,0,0,0,0), byrow=T, nrow=3)
for(i in 1:3) 
  for (j in 1:2) 
p[i,j]<-exp(d.coef[1]+d.coef[2]*i+d.coef[3]*j)/(1+exp(d.coef[1]+d.coef[2]*i+d.coef[3]*j))

print("Probability of Casting Vote at the Polls")
rownames(p)<-c("Young","Middle-Aged","Old")
colnames(p) <-c("Male","Female")
p






par(mfrow=c(1,2))
hist(fl.demo$vh10g,fre=FALSE)
hist(fl.rep$vh10g,fre=FALSE)

density(fl.demo$vh10g)







