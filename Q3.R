#Q3
if (!require("pacman")) install.packages("pacman")
p_load(rio)

Q3 <- import("~/R/data_assign/Dataset Q3 - student.csv")

if (!require("dplyr")) install.packages("dplyr")

Q3 <-rename(Q3,no=stud.id)
Q3 <-rename(Q3,stud.id=name)
Q3 <-rename(Q3,name=gender)
Q3 <-rename(Q3,gender=age)
Q3 <-rename(Q3,age=height)
Q3 <-rename(Q3,height=weight)
Q3 <-rename(Q3,weight=religion)
Q3 <-rename(Q3,religion=nc.score)
Q3 <-rename(Q3,nc.score=semester)
Q3 <-rename(Q3,semester=major)
Q3 <-rename(Q3,major=minor)
Q3 <-rename(Q3,minor=score1)
Q3 <- rename(Q3,score1=score2)
Q3 <-rename(Q3,score2=online.tutorial)
Q3 <-rename(Q3,online.tutorial=graduated)
Q3 <-rename(Q3,graduated=salary)
Q3 <- rename(Q3,salary=V17)


if(!require("ggplot2")) install.packages("ggplot2")

#1
ggplot()+geom_histogram(data = Q3,aes(x=height),fill="white",col = "red")+ggtitle("Histogram of height")+theme(plot.title = element_text(hjust = 0.5))

#2
 ggplot()+geom_histogram(data = Q3,aes(x=height,color=gender),fill="white")+ggtitle("Histogram of height")+theme(plot.title = element_text(hjust = 0.5))

#3	
maleMean = mean(Q3[Q3$height & Q3$gender == "Male","height"])
maleSd = sd(Q3[Q3$height & Q3$gender == "Male","height"])
femaleMean = mean(Q3[Q3$height & Q3$gender == "Female","height"])
femaleSd = sd(Q3[Q3$height & Q3$gender == "Female","height"])


#4
pnorm(161,femaleMean,femaleSd)

#5
(1-pnorm(170,femaleMean,femaleSd))

#6
(pnorm(160,femaleMean,femaleSd)-pnorm(150,femaleMean,femaleSd))

#7
(pnorm(180,femaleMean,femaleSd)-pnorm(170,femaleMean,femaleSd))

#9 
qnorm(0.25,femaleMean,femaleSd)

#10
qnorm(0.25,maleMean,maleSd)

rm(list=ls())
dev.off()
p_unload(all)


