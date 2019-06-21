#T1-
HTN.1<-c("no","no","yes","no","no","yes","no","no","no","yes")
# this is a categorical variable.
table(HTN.1)
prop.table(table(HTN.1))
#T2-
V2<-c(119,125,145,138,127,149,118,129,128,151)
#this is a continuous variable 
mean(V2)
#mean=132.9
sd(V2)
#sd=12.068
#T3-
edu.lev<-c(3,2,2,2,1,1,2,3,1,2)
#this is a categorical variable
table(edu.lev)
prop.table(table(edu.lev))
#T4-
res<-c("rural","suburban","urban","urban","suburban","suburban","rural","urban","suburban","rural")
#this is a categorical variable
table(res)
prop.table(table(res))
#T5-
HTN.data<-data.frame(HTN.1,V2,edu.lev,res)
HTN.data
