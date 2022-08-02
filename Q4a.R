#Random sample for Question 4
#library(dplyr) 
#sampleData<-sample_n(Dataset_Q4,100); sampleData
#View(sampleData)

library(readxl)
sampleData <- read_excel("sampleData.xlsx")
View(sampleData)


#Question 4a.a
stdS3<-sd(sampleData$stmnt3); stdS3
n=100
error <- qnorm(0.85)*stdS3/sqrt(n); error
avgS3<-mean(sampleData$stmnt3); avgS3
left <- avgS3-error; left
right<- avgS3+error; right


#Question 4a.b
stdS3<-sd(sampleData$stmnt3); stdS3
n=100
error <- qnorm(0.90)*stdS3/sqrt(n); error
avgS3<-mean(sampleData$stmnt3); avgS3
left <- avgS3-error; left
right<- avgS3+error; right



#Question 4a.c
stdS3<-sd(sampleData$stmnt3); stdS3
n=100
error <- qnorm(0.95)*stdS3/sqrt(n); error
avgS3<-mean(sampleData$stmnt3); avgS3
left <- avgS3-error; left
right<- avgS3+error; right


#Question 4a.e
meansample<-mean(sampleData$stmnt3); meansample
averageS3 = 3.89
n=100
z <- (meansample-averageS3)/(sd(sampleData$stmnt3)/sqrt(n)); z
pnorm(-abs(z))

#Question 4a.f
meansample<-mean(sampleData$stmnt4); meansample
averageS4 = 4.05
n=100
z <- (meansample-averageS4)/(sd(sampleData$stmnt4)/sqrt(n)); z
pnorm(-abs(z))

#Question 4a.g
meansample<-mean(sampleData$stmnt5); meansample
averageS5 = 4.19
n=100
z <- (meansample-averageS5)/(sd(sampleData$stmnt5)/sqrt(n)); z
2*pnorm(-abs(z))