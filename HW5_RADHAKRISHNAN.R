#T1-
data1<-read.table(file="HA.data.csv",header=T,sep = ",",as.is=T)
#T2-
table(data1$attack)
prop.table(table(data1$attack))
#T3-
table(data1$smokes)
prop.table(table(data1$smokes))
#T4-
mean(data1$age)
sd(data1$age)
min(data1$age)
max(data1$age)
median(data1$age)
#T5-
mean(data1$bmi)
median(data1$bmi)
min(data1$bmi)
max(data1$bmi)
sd(data1$bmi)
#T6-
table(data1$gender)
prop.table(table(data1$gender))
#T7-
table(data1$hsgrad)
prop.table(table(data1$hsgrad))
#T8-
table(data1$hightar)
prop.table(table(data1$hightar))
#T9-
hattack<-data1[which(data1$attack==1),]
hattack$smokes
table(hattack$smokes)
prop.table(table(hattack$smokes))
#out of all people who suffered heart attack, 59.32% are smokers.
nohattack<-data1[which(data1$attack==0),]
table(nohattack$smokes)
prop.table(table(nohattack$smokes))
#out of those who did not have heart attack, 24.61% are smokers.
#T10-
hattack$gender
table(hattack$gender)
prop.table(table(hattack$gender))
#out of those who had heart attacks, 27.11% are females.
nohattack$gender
table(nohattack$gender)
prop.table(table(nohattack$gender))
#out of those who did not have heart attack, 23.07% are females.
#T11-
hattack$hsgrad
table(hattack$hsgrad)
prop.table(table(hattack$hsgrad))
#out of those who had heart attack, 74.57% are high school graduates.
nohattack$hsgrad
table(nohattack$hsgrad)
prop.table(table(nohattack$hsgrad))
#out of those who did not have heart attacks, 72.30% are high school graduates.
#T12-
#the results from task 9-11 show that there is a major association between smoking and heart attack as more than half of the people who smoke have had heart attack. there is no strong association of people who had heart attack in relation to gender and high school graduation as there is no much difference in the proportions.
#T13-
new.data<-data1[,c("attack","age","bmi")]
write.table(new.data,file="new HA data.text",quote=F,sep ="\t",row.names = F,col.names = T)
