#T1-
datak<-read.table(file="HA.data.csv",header = TRUE,sep = ",",as.is = TRUE)
datak
#T2-
table(datak$hsgrad,datak$smokes)
prop.table(table(datak$hsgrad,datak$smokes),2)
# out of all the smokers, 68.6% are high school graduates.
#out of those who don't smoke,76.7% are high school graduates.
#T3-
table(datak$gender,datak$smokes)
prop.table(table(datak$gender,datak$smokes),2)
#25.4% are females in those who smoke.
#24.6% are females in those who don't smoke.
#T4-
#from T2 and T3, we can say that smoking has more association with high school graduation than gender as there is no much difference in proportions of females who smoke and who don't while there is around 8% difference in proportions of high school graduates who smoke and who don't smoke.
#T5-
hattack<-datak[which(datak$attack==1),]
nohattack<-datak[which(datak$attack==0),]
hattack
nohattack
hattack$age
mean(hattack$age)
sd(hattack$age)
nohattack$age
mean(nohattack$age)
sd(nohattack$age)
#mean age of persons with heart attack=59.03
#mean age of persons without heart attack=55.79
#the group with heart attack is older.
#T6
mean(hattack$bmi)
sd(hattack$bmi)
mean(nohattack$bmi)
sd(nohattack$bmi)
#mean BMI for those with heart attack=25.9%
#mean BMI for those without heart attack= 24.7%
#group with heart attack has more BMI.
#T7
hist(datak$age,breaks = 25,col = "green",main="histogram of age",xlab = "age in years")
#T8
boxplot(datak$bmi~datak$attack,col=c("blue","red"),main="BOXPLOT OF BMI BY HEART ATTACK STATUS",xlab="heart attack status",ylab="BMI",outline=FALSE,names=c("without heart attack","with heart attack"))
#T9
plot(datak$age,datak$bmi,main="age vs BMI",xlab="age",ylab="BMI",col="green")
