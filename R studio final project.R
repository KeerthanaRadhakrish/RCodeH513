#Q1
diab1<-read.table("diabetes1.csv",header=T,sep = ",",as.is = T)
diab2<-read.table("diabetes2.csv",header=T,sep=",",as.is=T)
head(diab1)
head(diab2)
dim(diab1)
dim(diab2)
ncol(diab1)
colnames(diab1)
colnames(diab1)[6]<-"county"
colnames(diab1)
diab<-rbind(diab1,diab2)
head(diab)
dim(diab$htn)
diab
summary(diab)
#Q2
mean(diab$chol)
hist(diab$chol)

mean(diab$stab.glu)
hist(diab$stab.glu)

mean(diab$hdl)
hist(diab$hdl)

mean(diab$age)
hist(diab$age)

mean(diab$height)
hist(diab$height)
diab$height[which(diab$height==-9)]<-NA
mean(diab$height)
mean(diab$height,na.rm=T)

mean(diab$weight)
hist(diab$weight)
table(diab$weight)
diab$weight[which(diab$weight==-9)]<-NA
mean(diab$weight)
mean(diab$weight,na.rm=T)

mean(diab$sbp)
hist(diab$sbp)
table(diab$sbp)
diab$sbp[which(diab$sbp==-9)]<-NA
mean(diab$sbp)
mean(diab$sbp,na.rm=T)

mean(diab$dbp)
hist(diab$dbp)
table(diab$dbp)
diab$dbp[which(diab$dbp==-9)]<-NA
mean(diab$dbp,na.rm=T)

mean(diab$waist)
hist(diab$waist)
table(diab$waist)

mean(diab$hip)
hist(diab$hip)
table(diab$hip)

mean(diab$glyhb)
hist(diab$glyhb)
table(diab$glyhb)
diab$glyhb[which(diab$glyhb==-9)]<-NA
mean(diab$glyhb,na.rm=T)

prop.table(table(diab$county))

prop.table(table(diab$gender))
table(diab$gender)

prop.table(table(diab$frame))
diab$frame[which(diab$frame=="U")]<-NA
diab$frame
table(diab$frame,exclude = NULL)
prop.table(table(diab$frame,exclude = NULL))

table(diab$height)
#Q3
#new variable "diabetic"
diab$diabetic<-NA
diab$diabetic[which(diab$glyhb>=6.5)]<-1
diab$diabetic[which(diab$glyhb<6.5)]<-0
diab$diabetic
table(diab$diabetic,exclude=NULL)


#creating new variable BMI from height and weight and conversion
diab$BMI<-diab$weight/diab$height^2*703
diab$BMI
#1 (hypertensive) if sbp > 140 OR dbp > 90
#0 (non-hypertensive) if sbp < 140 AND dbp < 90
diab$htn<-NA
diab$htn[which(diab$sbp>140|diab$dbp>90)]<-1
diab$htn[which(diab$sbp<=140 & diab$dbp<=90)]<-0
table(diab$htn,exclude=NULL)
head(diab)
#hip-waist ratio
diab$ratio<-diab$waist/diab$hip
diab$ratio
#wcat
##Underweight if bmi < 18.5
##Normal if 18.5 < bmi < 25
##Overweight if 25 < bmi < 30
##Obese if bmi > 30
diab$wcat<-NA
diab$wcat[which(diab$BMI<18.5)]<-"underweight"
diab$wcat[which(diab$BMI>18.5& diab$BMI<25)]<-"normal"
diab$wcat[which(diab$BMI>25 & diab$BMI<30)]<-"overweight"
diab$wcat[which(diab$BMI>30)]<-"obese"
table(diab$wcat,exclude = NULL)
head(diab)
diab $wcat[which (diab $ BMI < 18.5)] <- "underweight"
diab$BMI<-NULL
head(diab)
diab $ wcat <- NA
diab $ wcat [which (diab $ BMI < 18.5)] <- "underweight"
diab $ wcat [which (diab $ BMI >= 18.5 & diab $ BMI < 25)]<- "normal"
diab $ wcat [which (diab $ BMI >= 25 & diab $ BMI < 30)] <- "overweight"
diab $ wcat [which (diab $ BMI >= 30)] <- "obese"
table (diab $ wcat , exclude = NULL)
head (diab)
diab $ dbp [which (diab $ dbp == - 9)] <- NA
diab $ BMI <- diab $ weight / diab $ height ^ 2 * 703
diab $ htn [which (diab $ sbp > 140 | diab $ dbp > 90)] <- 1
diab $ htn [which (diab $ sbp <= 140 & diab $ dbp <= 90)] <- 0

#part 2
##Q4-
table(diab$glyhb,diab$frame)
large<-diab[which(diab$frame=="large"),]
medium<-diab[which(diab$frame=="medium"),]
small<-diab[which(diab$frame=="small"),]
mean(large$glyhb,na.rm=T)
sd(large$glyhb,na.rm=T)
mean(medium$glyhb,na.rm=T)
sd(medium$glyhb,na.rm=T)
mean(small$glyhb,na.rm=T)
sd(small$glyhb,na.rm=T)
##4. With a graph of your choice, display the distribution of glycosylated hemoglobin stratified by body frame size. What do you notice? Summarize your findings in less than 3 sentences. (4 pts)
boxplot(diab$glyhb~diab$frame,outline=F,main="boxplot of glycosylated hb(%) in relation to body frame size",xlab="body frame size",ylab="glycosylated hb%",col=c("red","orange","yellow")) 
summary(large$glyhb)
summary(medium$glyhb)
max(medium$glyhb,na.rm=T)
max(large$glyhb,na.rm=T)
summary(small$glyhb)
#according to the box plot showing the relation between glycosylated hb% and body frame size, the individuals with larger body frame tend to have more gly.hb %.
##median of gly.hb% in ppl with large frame size is slightly more than 5.more people are above the median;minimum
quantile(large $ glyhb,c(1,3)/4,na.rm=T)
median(large$glyhb,na.rm=T)
quantile(medium $ glyhb,c(1,3)/4,na.rm=T)
median(medium$glyhb,na.rm=T)
quantile(small$glyhb,c(1,3)/4,na.rm=T)
median(small$glyhb,na.rm=T)
boxplot ( diab $ glyhb ~ diab $ frame , outline = F , main = " BOXPLOT SHOWING RELATIONSHIP BETWEEN GLYCOSYLATED HAEMOGLOBIN (%) AND BODY FRAME SIZE", xlab = "Body Frame size", ylab = "Glycosylated HB% ", col = c("red","orange","yellow"), cex.main = 0.7)
##5.Assess the relationship between diabetic and wcat. Summarize your findings in less than 3 sentences and provide evidence to support your findings. (4 pts)
table(diab$sugar,diab$wcat,exclude=NULL)
prop.table(table(diab$diabetic,diab$wcat,exclude=NULL),2)
table(diab$sugar,diab$wcat, exclude=NULL)
##based on the table produced, in people with normal weight, 88.4% don't have diabetes while only 0.07% have diabetes.
## in people who are underweight, no one has diabetes.
## in people who are over-weight, 18.6% have diabetes and 78% don't have diabetes.
##in people who are obese, 20.3% have diabetes and 76% don't have diabetes.
###the table prepared from the given data, ..........
table (diab $ diabetic, diab $ wcat , exclude = NULL)
prop.table ( table ( diab $ diabetic , diab $ wcat,exclude = NULL), 2 )
table ( diab $ diabetic , diab $ wcat , exclude = NULL ) 



#q6
#Assess the relationship between diabetic and htn.  Does the relationship between diabetic and htn change by gender?
s<-data.frame(diab$sugar,diab$htn,diab$gender)
head(s)
dim(s)
table(diab$diabetic,diab$htn,exclude=NULL)
prop.table(table(diab$sugar,diab$htn,exclude=NULL),1)
s <- data.frame (diab $ diabetic , diab $ htn , diab $ gender)
diab $ htn [which (diab $ sbp > 140 | diab $ dbp > 90)] <- 1
diab $ htn [which (diab $ sbp <= 140 & diab $ dbp <= 90)] <- 0
table (diab $ htn,diab$diabetic,exclude=NULL)
prop.table (table (diab $ diabetic , diab $ htn, exclude = NULL) , 1)
female <- diab [which ( diab $ gender == "female") , ]
male <- diab [which ( diab $ gender == "male") , ]
prop.table (table (female $ diabetic , female $ htn , exclude = NULL) , 1 )
prop.table (table (male $ diabetic , male $ htn , exclude = NULL) , 1 )

## excluding the missing values, out of all the diabetics, 63.07% have hypertension and 30.7% don't have hypertension.
##out of those without diabetes, 36% have hypertension while 49.84% don't have hypertension.
##this shows that there is positive association between diabetes and hypertension.
female<-diab[which(diab$gender=="female"),]
male<-diab[which(diab$gender=="male"),]
prop.table(table(female$sugar,female$htn))
prop.table(table(female$sugar,female$htn,exclude=NULL),1)
prop.table(table(male$sugar,male$htn,exclude=NULL),1)
## among females with diabetes,63.8% have hypertension while 27.7% don't have hypertension.
## among females without diabetes,33.8% have hypertension while 52.6% dont have hypertension.
## among males with diabetes,62.06% have hypertension while 34.4% dont have hypertension.
## among male without diabetes,39.09% have hypertension while 45.8% dont have hypertension.
## this shows that there is no much effect of gender on relation between diabetes and hypertension.

#Q7
##total cholesterol, stabilized glucose, and high density lipoprotein-has the strongest association with glycosylated hemoglobin levels.  Provide evidence from your analysis to support your findings. 
c7<-data.frame(diab$glyhb,diab$chol,diab$stab.glu,diab$hdl)
head(c7)
##relationship between glyhb and chol.
plot(diab$glyhb,diab$chol,main="relation between glycosylated hb(%) and total cholesterol levels(mg/dL)",col="red")
plot(diab$glyhb,diab$stab.glu,main="relation between glycosylated hb(%) and stabilized glucose(mg/dL)",col="green")
plot (diab$glyhb,diab$hdl,main="relation between glycosylated hb(%) and high density lipoprotein(mg/dL)",col="purple")
par(mfrow=c(1,3))
plot(diab$glyhb,diab$chol,main="glycosylated hb(%) and total cholesterol(mg/dL)",col="red",cex.main=0.7)
abline(lm(diab$chol~diab$glyhb),col="black",lwd=1)
plot(diab$glyhb,diab$stab.glu,main="glycosylated hb(%) and stabilized glucose(mg/dL)",col="green",cex.main=0.7)
abline(lm(diab$stab.glu~diab$glyhb,col="black",lwd=1))
plot (diab$glyhb,diab$hdl,main="glycosylated hb(%) and high density lipoprotein(mg/dL)",col="purple",cex.main=0.7)
abline(lm(diab$hdl~diab$glyhb,col="black",lwd=1))
##based on the scatter plots presented, stabilized glucose levels have strongest relationship with glycosylated haemoglobin levels while total cholesterol has a weaker positive relationship and hgh density lipoprotein has a negative relationship with glycosylated hb.
par ( mfrow = c(1,3) )
plot ( diab $ glyhb , diab $ chol , main = "glycosylated hb(%) and total cholesterol(mg/dL)" ,xlab="glycosylated hb%",ylab="total cholesterol levels(mg/dL)",col = "red" , cex.main = 0.7)
abline ( lm ( diab $ chol ~ diab $ glyhb ))
plot ( diab $ glyhb , diab $ stab.glu , main = "glycosylated hb(%) and stabilized  glucose(mg/dL)",xlab = "glycosylated hb%",ylab = "stabilized glucose(mg/dL)",col="green" , cex.main = 0.7)
abline ( lm ( diab $ stab.glu ~ diab $ glyhb))
plot ( diab $ glyhb , diab $ hdl , main = "glycosylated hb(%) and high density lipoprotein(mg/dL)",xlab="glycosylated hb%",ylab="high density lipoprotein(mg/dL)",col="purple" , cex.main = 0.7)
abline ( lm ( diab $ hdl ~ diab $ glyhb))
par ( mfrow = c(1,3) )

plot ( diab $ glyhb , diab $ chol , main = " i ) glycosylated hb(%) and total cholesterol(mg/dL)" , xlab = "glycosylated hb% " , ylab = "total cholesterol levels(mg/dL)" , col = "red" , cex.main = 0.7 )
abline ( lm ( diab $ chol ~ diab $ glyhb ))

plot ( diab $ glyhb , diab $ stab.glu , main = " ii ) glycosylated hb(%) and stabilized  glucose(mg/dL)" , xlab = "glycosylated hb%" , ylab = "stabilized glucose(mg/dL)" , col = "green" , cex.main = 0.7 )
abline ( lm ( diab $ stab.glu ~ diab $ glyhb))

plot ( diab $ glyhb , diab $ hdl , main = " iii ) glycosylated hb(%) and high density lipoprotein(mg/dL)" , xlab = "glycosylated hb%" , ylab = "high density lipoprotein(mg/dL)" , col = "purple" , cex.main = 0.7 )
abline ( lm ( diab $ hdl ~ diab $ glyhb))



#Q8
#Explore the relationship between glycosylated hemoglobin, BMI, and gender
a<-data.frame(diab$glyhb,diab$BMI,diab$gender)
head(a)
summary(a)
female$glyhb
par(mfrow=c(1,3))
plot(female$glyhb,female$BMI,main="glycosylated hb(%) and BMI(pounds/inch.sq.)in females",xlab="glycosylated hb%", ylab="BMI(pounds/sq.Inches)",cex.main=0.7,col="magenta")
abline(lm(female$BMI~female$glyhb,lwd=1,col="black"))
plot(male$glyhb,male$BMI,main="glycosylated haemoglobin(%) and BMI(pounds/inch.sq) in males",xlab="glycosylated hb%", ylab="BMI(pounds/sq.Inches)",cex.main=0.7,col="blue")
abline(lm(male$BMI~male$glyhb,lwd=1,col="black"))
##..................................
par ( mfrow = c(1,3) )
plot ( female $ glyhb , female $ BMI , main = "glycosylated hb(%) and BMI(pounds/inches sq.)in females " ,xlab="glycosylated hb %", ylab="BMI", cex.main = 0.7 , col = "magenta" )
abline ( lm ( female $ BMI ~ female $ glyhb) )
plot ( male $ glyhb , male $ BMI , main = "glycosylated hemoglobin(%) and BMI(pounds/inch.sq) in males",xlab="glycosylated hb%",ylab = "BMI" , cex.main = 0.7 , col = "blue" )
abline ( lm ( male $ BMI ~ male $ glyhb) )

#Q9
#Explore the data and identify two additional risk factors for glycosylated hemoglobin (glyhb) or diabetes status (diabetes).  For each risk factor identified, present evidence from your analysis to support your findings. Additionally, determine whether the effect of each risk factor changes by gender. 
head(diab)
q9<-data.frame(diab$sugar,diab$age,diab$ratio)

head(q9)
plot(diab$glyhb,diab$age,main="SCATTERPLOT OF GLYCOSYLATED HB% IN RELATION TO AGE",col="violetred",xlab="glycosylated hb (%)", ylab="age (in years),cex=0.7")
abline(lm(diab$age~diab$glyhb))
plot(diab$glyhb,diab$ratio,main="SCATTERPLOT OF GLYCOSYLATED HB% IN RELATION TO HIP-WAIST RATIO",xlab="glycosylated hb(%)",ylab="hip-waist ratio", col="deepskyblue2",cex.main=0.5)
abline(lm(diab$ratio~diab$glyhb))

#  glyhb$agein relation to gender
q9<-diab[,c("glyhb","age","gender")]
head(q9)
ggplot(diab,aes(x=age,y=glyhb,col=gender), addRegLine=TRUE)+ geom_point(alpha = 0.8)

par(mfrow=c(1,2))
plot(female$glyhb,female$age,main="SCATTERPLOT OF GLYCOSYLATED HB% IN RELATION TO AGE IN FEMALES",cex.main=0.5,col="chocolate3",xlab="glycosylated hb (%)", ylab="age (in years)")
abline(lm(female$age~female$glyhb))
plot(male$glyhb,male$age,main="SCATTERPLOT OF GLYCOSYLATED HB% IN RELATION TO AGE IN MALES", cex.main=0.5,col="darkolivegreen4",xlab="glycosylated hb (%)",ylab="age (in years)")
abline(lm(male$age~male$glyhb))
plot(diab$glyhb,diab$age,main="SCATTERPLOT OF GLYCOSYLATED HB% IN RELATION TO AGE",col="violetred",xlab="glycosylated hb (%)", ylab="age (in years)",cex.main=0.5)
abline(lm(diab$age~diab$glyhb))
plot(diab$glyhb,diab$ratio,main="SCATTERPLOT OF GLYCOSYLATED HB% IN RELATION TO HIP-WAIST RATIO",xlab="glycosylated hb(%)",ylab="hip-waist ratio", col="deepskyblue2",cex.main=0.5)
abline(lm(diab$ratio~diab$glyhb))
# glyhb$ratio in relation to gender
par(mfrow=c(1,2))
plot(female$glyhb,female$ratio,main="SCATTERPLOT OF GLYCOSYLATED HB% IN RELATION TO HIP-WAIST RATIO IN FEMALES",cex.main=0.45,col="darkred",xlab="glycosylated hb (%)", ylab="hip-waist ratio")
abline(lm(female$ratio~female$glyhb))
plot(male$glyhb,male$ratio,main="SCATTERPLOT OF GLYCOSYLATED HB% IN RELATION TO HIP-WAIST RATIO IN MALES", cex.main=0.45,col="cadetblue",xlab="glycosylated hb (%)",ylab="hip-waist ratio")
abline(lm(male$ratio~male$glyhb))

plot( diab $ glyhb , diab $ age , main= "SCATTERPLOT OF GLYCOSYLATED HB% IN RELATION TO AGE" , col= "violetred" , xlab= "glycosylated hb (%)", ylab="age (in years)",cex=0.8)
abline ( lm ( diab $ age ~ diab $ glyhb))
plot(diab $ glyhb , diab $ age , main= "SCATTERPLOT OF GLYCOSYLATED HB% IN RELATION TO AGE" , col= "violetred" , xlab= "glycosylated hb (%)", ylab="age (in years)",cex=0.8)
abline(lm (diab $ age ~ diab $ glyhb))

plot (diab $ glyhb , diab $ ratio , main= "SCATTERPLOT OF GLYCOSYLATED HB% IN RELATION TO HIP-WAIST RATIO" , xlab = "glycosylated hb(%)" , ylab= "hip-waist ratio" ,  col= "deepskyblue2" , cex.main=0.8)
abline ( lm ( diab $ ratio ~ diab $ glyhb ))

plot (diab $ glyhb , diab $ age , main= "SCATTERPLOT OF GLYCOSYLATED HB% IN RELATION TO AGE" , col = "violetred" , xlab = "glycosylated hb (%)" , ylab = "age (in years)",cex = 0.8 )
abline ( lm ( diab $ age ~ diab $ glyhb ) )
       
plot (diab $ glyhb , diab $ ratio , main= "SCATTERPLOT OF GLYCOSYLATED HB% IN RELATION TO HIP-WAIST RATIO" , xlab = "glycosylated hb(%)" , ylab="hip-waist ratio" , col = "deepskyblue2" , cex.main = 0.8 )
abline ( lm ( diab $ ratio ~ diab $ glyhb ) )

# in relation to gender
plot (female $ glyhb , female $ age , main= "SCATTERPLOT OF GLYCOSYLATED HB% IN RELATION TO AGE IN FEMALES" , cex.main = 0.7 , col = "chocolate3" , xlab = "glycosylated hb (%)" , ylab = "age (in years)")
abline ( lm ( female $ age ~ female $ glyhb ) )

plot (male $ glyhb , male $ age , main= "SCATTERPLOT- GLYCOSYLATED HB(%) IN RELATION TO AGE IN MALES" , cex.main = 0.7 , col = "darkolivegreen4" , xlab = "glycosylated hb (in %)" , ylab = "age (in years)")
abline ( lm ( male $ age ~ male $ glyhb ) )



#q10
## Create a Table 1 that summarizes the following variables: glyhb, location, age, gender, bmi, htn, ratio, and wcat overall and stratified by diabetes status.  That is, summarize these variables for the two diabetes statuses separately.


##1
summary(diabet$glyhb)
sd(diabet$glyhb)
summary(diab$glyhb)
sd(diab$glyhb)
summary(nodiabet$glyhb)
sd(nodiabet$glyhb)
##2
summary(diabet$age)
sd(diabet$age)
summary(diab$age)
sd(diab$age)
summary(nodiabet$age)
sd(nodiabet$age)
##3
summary(diabet$BMI)
sd(diabet$BMI)
summary(diab$BMI)
sd(diab$BMI)
summary(nodiabet$BMI)
sd(nodiabet$BMI)
##4
summary(diabet$ratio)
sd(diabet$ratio)
summary(diab$ratio)
sd(diab$ratio)
summary(nodiabet$ratio)
sd(nodiabet$ratio)
##5
table(diabet$county,exclude= NULL)
prop.table(table(diabet$county,exclude= NULL))
table(diab$county,exclude= NULL)
prop.table(table(diab$county,exclude= NULL))
table(nodiabet$county,exclude= NULL)
prop.table(table(nodiabet$county,exclude= NULL))
##6
table(diabet$gender,exclude= NULL)
prop.table(table(diabet$gender,exclude= NULL))
table(diab$gender,exclude= NULL)
prop.table(table(diab$gender,exclude= NULL))
table(nodiabet$gender,exclude= NULL)
prop.table(table(nodiabet$gender,exclude= NULL))
##7
table(diabet$htn,exclude= NULL)
prop.table(table(diabet$htn,exclude= NULL))
table(diab$htn,exclude= NULL)
prop.table(table(diab$htn,exclude= NULL))
table(nodiabet$htn,exclude= NULL)
prop.table(table(nodiabet$htn,exclude= NULL))
##8
table(diabet$wcat,exclude= NULL)
prop.table(table(diabet$wcat,exclude= NULL))
table(diab$wcat,exclude= NULL)
prop.table(table(diab$wcat,exclude= NULL))
table(nodiabet$wcat,exclude= NULL)
prop.table(table(nodiabet$wcat,exclude= NULL))
##1
summary ( diabet $ glyhb )
sd ( diabet $ glyhb )
summary ( diab $ glyhb )
sd (diab $ glyhb,na.rm=T)
summary ( nodiabet $ glyhb)
sd (nodiabet $ glyhb)
##2
summary (diabet $ age)
sd (diabet $ age)
summary ( diab $ age)
sd (diab $ age)
summary (nodiabet $ age)
sd (nodiabet $ age)
##3
summary (diabet $ BMI)
sd (diabet $ BMI,na.rm=T)
summary (diab $ BMI)
sd (diab $ BMI,na.rm=T)
summary (nodiabet $ BMI)
sd (nodiabet $ BMI,na.rm=T)
##4
summary (diabet $ ratio)
sd (diabet $ ratio)
summary(diab $ ratio)
sd (diab $ ratio)
summary (nodiabet $ ratio)
sd (nodiabet $ ratio)
##5
table (diabet $ county , exclude = NULL)
prop.table (table (diabet $ county , exclude = NULL))
table (diab $ county , exclude = NULL)
prop.table (table (diab $ county , exclude = NULL))
table (nodiabet $ county , exclude = NULL)
prop.table (table (nodiabet $ county , exclude = NULL))
##6
table (diabet $ gender , exclude = NULL)
prop.table (table (diabet $ gender , exclude = NULL))
table (diab $ gender , exclude = NULL)
prop.table (table (diab $ gender , exclude = NULL))
table (nodiabet $ gender , exclude = NULL)
prop.table (table (nodiabet $ gender , exclude = NULL))
##7
table (diabet $ htn , exclude = NULL)
prop.table ( table ( diabet $ htn , exclude = NULL))
table (diab $ htn , exclude = NULL)
prop.table (table (diab $ htn , exclude = NULL))
table (nodiabet $ htn , exclude= NULL)
prop.table (table (nodiabet $ htn , exclude = NULL))
##8
table (diabet $ wcat , exclude = NULL)
prop.table (table (diabet $ wcat , exclude = NULL))
table (diab $ wcat , exclude = NULL)
prop.table (table (diab $ wcat , exclude = NULL))
table (nodiabet $ wcat , exclude = NULL)
prop.table (table (nodiabet $ wcat , exclude = NULL))


diab $ diabetic <- NA
diab $ diabetic [which (diab $ glyhb >= 6.5)] <- 1
diab $ diabetic [which (diab $ glyhb < 6.5)] <- 0
diab $ diabetic
table (diab $ diabetic , exclude = NULL)
diabet<-diab[which(diab$diabetic==1),]
nodiabet<-diab[which(diab$diabetic==0),]
diabet<- diab[which(diab $ diabetic ==1) , ]
nodiabet<- diab[which(diab $ diabetic == 0) , ]

