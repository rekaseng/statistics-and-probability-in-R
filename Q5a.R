#Q5
if (!require("pacman")) install.packages("pacman")
p_load(rio)

Q5 <- import("~/R/data_assign/Dataset Q5 - Daily Rainfall 2020.xlsx")

library(dplyr)


Dataset1 <- filter(Q5, State == "NSembilan")
Dataset2 <- filter(Q5, State == "Pahang")
Dataset3 <- filter(Q5, State == "Penang")
Dataset4 <- filter(Q5, State == "Perak")
Dataset5 <- filter(Q5, State == "Perlis")


Dataset1_Nov_Dec <- Dataset1[Dataset1$Month == 11 | Dataset1$Month == 12, ]
Dataset2_Nov_Dec <- Dataset2[Dataset2$Month == 11 | Dataset2$Month == 12,]
Dataset3_Nov_Dec <- Dataset3[Dataset3$Month == 11 | Dataset3$Month == 12,]
Dataset4_Nov_Dec <- Dataset4[Dataset4$Month == 11 | Dataset4$Month == 12,]
Dataset5_Nov_Dec <- Dataset5[Dataset5$Month == 11 | Dataset5$Month == 12,]

m1 <- mean(Dataset1_Nov_Dec$`Rainfall (mm)`)
sd1 <- sd(Dataset1_Nov_Dec$`Rainfall (mm)`)

m2 <- mean(Dataset2_Nov_Dec $`Rainfall (mm)`)
sd2 <- sd(Dataset2_Nov_Dec $`Rainfall (mm)`)

m3 <- mean(Dataset3_Nov_Dec $`Rainfall (mm)`)
sd3 <- sd(Dataset3_Nov_Dec$`Rainfall (mm)`)

m4 <- mean(Dataset4_Nov_Dec$`Rainfall (mm)`)
sd4 <- sd(Dataset4_Nov_Dec$`Rainfall (mm)`)

m5 <- mean(Dataset5_Nov_Dec$`Rainfall (mm)`)
sd5 <- sd(Dataset5_Nov_Dec$`Rainfall (mm)`)


boxplot(Dataset1_Nov_Dec$`Rainfall (mm)`, main="NSembilan rainfall",xlab ="NSembilan",ylab="Rainfall (mm)")

boxplot(Dataset2_Nov_Dec $`Rainfall (mm)`,main="Pahang rainfall",xlab="Pahang",ylab="Rainfall (mm)")

boxplot(Dataset3_Nov_Dec $`Rainfall (mm)`,main="Penang rainfall",xlab="Penang",ylab="Rainfall (mm)")

boxplot(Dataset4_Nov_Dec$`Rainfall (mm)`,main="Perak rainfall",xlab="Perak",ylab="Rainfall (mm)")

boxplot(Dataset5_Nov_Dec$`Rainfall (mm)`,main="Perlis rainfall",xlab="Perlis",ylab="Rainfall (mm)")

# 5i
error <- qnorm(0.975)*sd1/sqrt(61)
m1-error
m1+error

# 5ii
error <- qnorm(0.99)*sd1/sqrt(61)
m1-error
m1+error

# 5 iii
x = (Dataset2_Nov_Dec $`Rainfall (mm)`)
y = (Dataset3_Nov_Dec $`Rainfall (mm)`)
t.test(x,y,alternative="greater",conf.level = 0.95,var.equal = TRUE)

# 5 iv
x = (Dataset4_Nov_Dec$`Rainfall (mm)`)
y = (Dataset5_Nov_Dec$`Rainfall (mm)`)


t.test(x,y,alternative="two.sided",conf.level = 0.975)

rm(list=ls())
dev.off()
p_unload(all)