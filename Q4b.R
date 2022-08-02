#Question 4b
pmean<-mean(Dataset_Q4b_Price_of_houses_in_Malaysia$`Price (RM)`); pmean
#[1] 572003.1
Pstd<-sd(Dataset_Q4b_Price_of_houses_in_Malaysia$`Price (RM)`); Pstd
#[1] 221829.6

johorhouses<-subset(Dataset_Q4b_Price_of_houses_in_Malaysia, State=='Johor')
View(johorhouses)
johormean<-mean(johorhouses$`Price (RM)`); johormean
johorstd<-sd(johorhouses$`Price (RM)`); johorstd

#Question 4b.a
Pstd<-sd(Dataset_Q4b_Price_of_houses_in_Malaysia$`Price (RM)`)
n=100
error <- qnorm(0.90)*Pstd/sqrt(n); error
johormean<-mean(johorhouses$`Price (RM)`)
left <- johormean-error; left
right<- johormean+error; right


#Question 4b.b
johormean<-mean(johorhouses$`Price (RM)`); johormean
pmean<-mean(Dataset_Q4b_Price_of_houses_in_Malaysia$`Price (RM)`); pmean
n=100
z <- (johormean-pmean)/(sd(Dataset_Q4b_Price_of_houses_in_Malaysia$`Price (RM)`)/sqrt(n)); z

#Question 4b.c
n = length(johorhouses$`Price (RM)`); n # valid responses count
k = sum(johorhouses$`Price (RM)`<600000); k
pbar = k/n; pbar 
nforMalaysiahouseprice = length(Dataset_Q4b_Price_of_houses_in_Malaysia$`Price (RM)`); nforMalaysiahouseprice
kforMalaysiahouseprice = sum(Dataset_Q4b_Price_of_houses_in_Malaysia$`Price (RM)`<600000); kforMalaysiahouseprice
p = kforMalaysiahouseprice/nforMalaysiahouseprice; p


#Question 4b.d
SE = sqrt(pbar*(1-pbar)/n); SE     # standard error
E = qnorm(.975)*SE; E              # margin of error 
pbar + c(-E, E) 

#Question 4b.e
p=0.4
n = length(johorhouses$`Price (RM)`); n
pbar = k/n; pbar
z= pbar-p/ sqrt((p*(1-p))/n);z



