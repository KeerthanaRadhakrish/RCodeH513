#T1
HAnew<-read.table("HA.data.updated.txt",header = T,sep=" ",as.is=T)
#T2
mean(HAnew$age)
mean(HAnew$bmi)
mean(HAnew$sbp)
#in the data given, there are missing values presentin continuous variables like age and bmi but sbp has no such missing values.we can determine this by calculating the mean values. if the mean is negative, then, we can say that there are missing values in the data.
hist(HAnew$age)
hist(HAnew$bmi)
hist(HAnew$sbp)
#histogram is another way to find out missing values.If the histogram shows negative values in the x axis, the data is said to have missing values.
HAnew$age[which(HAnew$age==-999)]<-NA
HAnew$bmi[which(HAnew$bmi==-999)]<-NA
HAnew
#T3
prop.table(table(HAnew$attack))
prop.table(table(HAnew$smokes))
prop.table(table(HAnew$gender))
prop.table(table(HAnew$hsgrad))
prop.table(table(HAnew$hightar))
#in the given data, the categorical variables smokes,hightar have missing values. this can be determined by finding the proportion of these variables. if R finds proportion for any other value other than 0 or 1 (here), then the variable has missing values.
HAnew$smokes[which(HAnew$smokes==-9)]<-NA
HAnew$hightar[which(HAnew$hightar==-9)]<-NA
HAnew
#T4
mean(HAnew$age,na.rm = T)
median(HAnew$age,na.rm=T)
hist(HAnew$age,main="histogram of age",xlab = "age in years",ylab="frequency")
abline(v=57.33,col="orange")
abline(v=55.4,col="green",lty=3,lwd=1)
legend(72,35,legend=c("median age","mean age"),pch=1,col=c("green","orange"))
#T5
plot(HAnew$age,HAnew$sbp,main ="age vs sbp",xlab = "age in years",ylab = "sbp in mmHg",col="blue",cex=0.7)
abline(lm(HAnew$sbp~HAnew$age),col="black",lty=2,lwd=4)
# the plot shows that there is no much association between age ad systolic blood pressure as the best-fit line here is more a linear line.
#T6
plot(HAnew$bmi,HAnew$sbp,main="BMI vs SBP",xlab="BMI in kg/m.sq",ylab="SBP in mm Hg",col="yellow",cex=0.9)
points(HAnew$bmi[which(HAnew$attack==1)],HAnew$sbp[which(HAnew$attack==1)],col="red",cex=0.9)
points(HAnew$bmi[which(HAnew$attack==0)],HAnew$sbp[which(HAnew$attack==0)],col="blue",cex=0.9)
abline(lm(HAnew$sbp~HAnew$bmi),col="black",lty=1,lwd=1)
# people with more systolic BP have more attacks.
# people with heart attacks have BMI between 20 and 30 kg/m.sq. 
#The plot doesn't show much association between BMI and systolic blood pressure as the bestfit line here is more a linear line.

