ToothGrowth
#T1-
nrow(ToothGrowth)
#T2-
table(ToothGrowth$supp)
#T3-
table(ToothGrowth$dose)
#T4-
min(ToothGrowth$len)
max(ToothGrowth$len)
median(ToothGrowth$len)
mean(ToothGrowth$len)
#T5-
(ToothGrowth$len)[c(31:60)]
mean((ToothGrowth$len)[c(31:60)])
#T6-
(ToothGrowth$len)[c(1:30)]
mean((ToothGrowth$len)[c(1:30)])
#T7-
subset.OJ<-which(ToothGrowth$supp=="OJ")
subset.OJ
table(subset.OJ)
subset.VC<-which(ToothGrowth$supp!="OJ")
subset.VC
#T8-
mean(ToothGrowth$len[which(ToothGrowth$supp=="OJ" & ToothGrowth$dose==0.5)])
mean(ToothGrowth$len[which(ToothGrowth$supp=="OJ"&ToothGrowth$dose==1)])
mean(ToothGrowth$len[which(ToothGrowth$supp=="OJ"&ToothGrowth$dose==2)])
#T9-
mean(ToothGrowth$len[which(ToothGrowth$supp=="VC"&ToothGrowth$dose==0.5)])
mean(ToothGrowth$len[which(ToothGrowth$supp=="VC" &ToothGrowth$dose==1)])
mean(ToothGrowth$len[which(ToothGrowth$supp=="VC"&ToothGrowth$dose==2)])
#T10-
#here, out of 60 people, 30 people ae given orange juice and 30 are given ascorbic acid.out of 30, 10 each are given 0.5, 1 and 2 mg/ day. according to the data, the length of odontoblasts is more in orange juice than ascorbic acid when given in 0.5 and 1 mg. but, when given in 2 mg dosage per day, the ascorbic acid causes more growth of odontoblasts than orange juice.