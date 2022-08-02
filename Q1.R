#Question 1.2a a.	Display all the data in a table,
#then summarize it with frequency details. 

#to rename the original dataset(Dataset_Q1_MoneySpending)
names(Dataset_Q1_MoneySpending)[names(Dataset_Q1_MoneySpending) == "To answer Q1,you can choose ONE dataset from these 2 : Set1 or Set2"] <- "Dataset"
names(Dataset_Q1_MoneySpending)[names(Dataset_Q1_MoneySpending) == "Total allowance you received for one semester =5months (from PTPTN/scholarship/study loan/etc + parents + etc)"] <- "Variable-A"
names(Dataset_Q1_MoneySpending)[names(Dataset_Q1_MoneySpending) == "Total spending on rental (room/house/ hostel), for 1 month" ] <- "Variable-B1"
names(Dataset_Q1_MoneySpending)[names(Dataset_Q1_MoneySpending) == "Total spending on meal/food/ groceries, for 1 month" ] <- "Variable-B2"
names(Dataset_Q1_MoneySpending)[names(Dataset_Q1_MoneySpending) == "Total spending on transportation, for 1 month"] <- "Variable-B3"
names(Dataset_Q1_MoneySpending)[names(Dataset_Q1_MoneySpending) == "Total spending on bill & utilities (electricity/water/TV/phone/internet/etc), for 1 month"] <- "Variable-B4"
names(Dataset_Q1_MoneySpending)[names(Dataset_Q1_MoneySpending) == "Total spending on saving of an emergency fund, for 1 month"] <- "Variable-B5"
names(Dataset_Q1_MoneySpending)[names(Dataset_Q1_MoneySpending) == "Total spending on saving of loan repayment (if any), for 1 month"] <- "Variable-B6"


#to filter the original dataset(Dataset_Q1_MoneySpending becomes df2)

df2<-data.frame(Dataset_Q1_MoneySpending$Dataset,Dataset_Q1_MoneySpending$`Variable-A`, Dataset_Q1_MoneySpending$`Variable-B1`,Dataset_Q1_MoneySpending$`Variable-B2`,Dataset_Q1_MoneySpending$`Variable-B3`,Dataset_Q1_MoneySpending$`Variable-B4`,Dataset_Q1_MoneySpending$`Variable-B5`,Dataset_Q1_MoneySpending$`Variable-B6`)

View(df2)
colnames(df2)

#to rename the sub dataset(df2)

names(df2)[names(df2) == "Dataset_Q1_MoneySpending.Dataset" ] <- "Dataset"
names(df2)[names(df2) == "Dataset_Q1_MoneySpending..Variable.A." ] <- "Variable-A"
names(df2)[names(df2) == "Dataset_Q1_MoneySpending..Variable.B1." ] <- "Variable-B1"
names(df2)[names(df2) == "Dataset_Q1_MoneySpending..Variable.B2." ] <- "Variable-B2"
names(df2)[names(df2) == "Dataset_Q1_MoneySpending..Variable.B3." ] <- "Variable-B3"
names(df2)[names(df2) =="Dataset_Q1_MoneySpending...tVariable.B1."] <- "Variable-B1"
names(df2)[names(df2) =="Dataset_Q1_MoneySpending...tVariable.B2."] <- "Variable-B2"
names(df2)[names(df2) == "Dataset_Q1_MoneySpending..Variable.B4." ] <- "Variable-B4"
names(df2)[names(df2) == "Dataset_Q1_MoneySpending..Variable.B5." ] <- "Variable-B5"
names(df2)[names(df2) == "Dataset_Q1_MoneySpending..Variable.B6." ] <- "Variable-B6"

#to eliminate the sub dataset(df2 that contains 'Set2 -This semester @hometown -with MCO')

df2<-subset(df2, Dataset!='Set2 -This semester @hometown -with MCO')

View(df2)

#frequency table for each variable
FreqA<-table(df2$`Variable-A`, exclude = NULL)
View(FreqA)
FreqB1<-table(df2$`Variable-B1`, exclude = NULL)
View(FreqB1)
FreqB2<-table(df2$`Variable-B2`, exclude = NULL)
View(FreqB2)
FreqB3<-table(df2$`Variable-B3`, exclude = NULL)
View(FreqB3)
FreqB4<-table(df2$`Variable-B4`, exclude = NULL)
View(FreqB4)
FreqB5<-table(df2$`Variable-B5`, exclude = NULL)
View(FreqB5)
FreqB6<-table(df2$`Variable-B6`, exclude = NULL)
View(FreqB6)


#Question 1.2b b.	Plot a suitable graph for each variable. Explain the results. 
x=df2$`Variable-A`
hist(x,xlab = "Variable-A", ylab = "Frequency", main = "Variable-A Frequency")
x=df2$`Variable-B1`
hist(x,xlab = "Variable-B1", ylab = "Frequency", main = "Variable-B1 Frequency")
x=df2$`Variable-B2`
hist(x,xlab = "Variable-B2", ylab = "Frequency", main = "Variable-B2 Frequency")
x=df2$`Variable-B3`
hist(x,xlab = "Variable-B3", ylab = "Frequency", main = "Variable-B3 Frequency")
x=df2$`Variable-B4`
hist(x,xlab = "Variable-B4", ylab = "Frequency", main = "Variable-B4 Frequency")
x=df2$`Variable-B5`
hist(x,xlab = "Variable-B5", ylab = "Frequency", main = "Variable-B5 Frequency")
x=df2$`Variable-B6`
hist(x,xlab = "Variable-B6", ylab = "Frequency", main = "Variable-B6 Frequency")

#Question 1.2c 
sumofB<-sum(df2$`Variable-B1`,df2$`Variable-B2`,df2$`Variable-B3`,df2$`Variable-B4`,df2$`Variable-B5`,df2$`Variable-B6`)
squaresumB1<-sum((df2$`Variable-B1`)^2)
squaresumB2<-sum((df2$`Variable-B2`)^2)
squaresumB3<-sum((df2$`Variable-B3`)^2)
squaresumB4<-sum((df2$`Variable-B4`)^2)
squaresumB5<-sum((df2$`Variable-B5`)^2)
squaresumB6<-sum((df2$`Variable-B6`)^2)
squaresumB<-sum(squaresumB1, squaresumB2,squaresumB3,squaresumB4,squaresumB5,squaresumB6)
varianceB = (squaresumB/n)-((sumofB/n)^2)
stdB = sqrt(varianceB)
meanofB<-sumofB/n
#z value and P value for Variable B1
z=(mean(df2$`Variable-B1`)-meanofB)/stdB
pnorm(abs(z))
#0.7608168

#z value and P value for Variable B2
z=(mean(df2$`Variable-B2`)-meanofB)/stdB
pnorm(abs(z))
#0.6977585

#z value and P value for Variable B3
z=(mean(df2$`Variable-B3`)-meanofB)/stdB
pnorm(-abs(z))
#0.2911613

#z value and P value for Variable B4
z=(mean(df2$`Variable-B4`)-meanofB)/stdB
pnorm(-abs(z))
#0.4246725

#z value and P value for Variable B5
z=(mean(df2$`Variable-B5`)-meanofB)/stdB
pnorm(-abs(z))
#0.490215

#z value and P value for Variable B6
z=(mean(df2$`Variable-B6`)-meanofB)/stdB
pnorm(-abs(z))
#0.3218909

#Question 1.2d d.	For all variables, compute the expected values, variances, 
#and standard deviation accordingly. Explain the results. 
mean(df2$`Variable-A`)
#[1] 991.4634
var(df2$`Variable-A`)
#[1] 3167613
sd(df2$`Variable-A`)
#[1] 1779.779
mean(df2$`Variable-B1`)
#[1] 181.8293
var(df2$`Variable-B1`)
#[1] 22269.7
sd(df2$`Variable-B1`)
#[1] 149.2303
mean(df2$`Variable-B2`)
#[1] 162.6829
var(df2$`Variable-B2`)
#[1] 8290.122
sd(df2$`Variable-B2`)
#[1] 91.05011
mean(df2$`Variable-B3`)
#[1] 55.60976
var(df2$`Variable-B3`)
#[1] 2005.244
sd(df2$`Variable-B3`)
#[1] 44.77995
mean(df2$`Variable-B4`)
#[1] 91.70732
var(df2$`Variable-B4`)
#[1] 3984.512
sd(df2$`Variable-B4`)
#[1] 63.12299
mean(df2$`Variable-B5`)
#[1] 108.2927
var(df2$`Variable-B5`)
#[1] 8829.512
sd(df2$`Variable-B5`)
#[1] 93.96548
mean(df2$`Variable-B6`)
#[1] 64.39024
var(df2$`Variable-B6`)
#[1] 2800.244
sd(df2$`Variable-B6`)
#[1] 52.91733


