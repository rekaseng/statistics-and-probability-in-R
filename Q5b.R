#Question 5b

if (!require("pacman")) install.packages("pacman")
p_load(rio)

Q5b <- import("~/R/data_assign/Q5data.xlsx")

library(dplyr)
p_1 <- count(Q5b[Q5b$`Plan 1-Working with a company` == "YES",])

p_2 <- count(Q5b[Q5b$`Plan 2-Establish a business/own company` == "YES",])

p_3 <- count(Q5b[Q5b$`Plan 3-Further study / take professional courses` == "YES",])

 p_4 <- count(Q5b[Q5b$`Plan 4-Take a break for personal reasons (e.g: traveling, getting married, etc)` == "YES",])

# 5i) H0 : p_1 >= 0.429
#H1 : p_1 < 0.429
prop.test(x= p_1$n ,n = 60,p = 0.429, correct = FALSE, alternative = "less",conf.level = 0.95)

#5ii)
#H0 : p_2 <= 0.156
#H1 : p_2 > 0.156
prop.test(x=p_2$n, n=60, p=0.156,correct =  FALSE,alternative = "greater",conf.level = 0.99)

#5iii)
#H0 : p_3 = 0.338
#H1 : p_3 != 0.338
prop.test(x=p_3$n, n=60, p=0.338,correct =  FALSE,alternative = "two.sided",conf.level = 0.95)


#5iv)
#H0 : p_4 >= 0.044
#H1 : p_4 < 0.044
prop.test(x=p_4$n, n=60, p=0.044,correct =  FALSE,alternative = "less",conf.level = 0.9)


