#Question_02
#Question_02_(a)
#a
X~binom(n=31,p=0.447)
#b
plot(c(1:31),dbinom(c(1:31),size=31,prob=0.447),type="h",ylab="Probability Mass",xlab="Number of Success")
#c
plot(c(1:31),pbinom(c(1:31),size=31,prob=0.447),type="h",ylab="Cumulative Probability",xlab="Number of Success")
#d
dbinom( 17, size=31, prob=0.447)
#e
pbinom( 13, size= 31, prob= 0.447)
#f
pbinom( 11, size=31, prob= 0.447, lower.tail = FALSE )
#g
pbinom( 14, size = 31, prob = 0.447, lower.tail = FALSE)
#h
diff(pbinom(c(19,15), size=31, prob=0.447,lower.tail= FALSE))
#i
library(distrEx)
X = Binom(size = 31, prob =0.447)
E(X)
#j
var(X)
#k
sd(X)
#l
E(4* X + 51.324)

#Question_02_(b)
#probability for all are chicken
(choose(8,3)*choose(7,0))/choose(15,3)

#probability for all are shrimp
(choose(8,0)*choose(7,3))/choose(15,3)

#probability for all have the same filling
((choose(8,3)*choose(7,0))/choose(15,3))+((choose(8,0)*choose(7,3))/choose(15,3))




























