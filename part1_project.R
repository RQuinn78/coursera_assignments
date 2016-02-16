##Illustrate via simulation and associated explanatory text the properties of the distribution of the mean of 40 exponentials.  You should

##1. Show the sample mean and compare it to the theoretical mean of the distribution.
avg_exp <- NULL
for (i in 1:1000) avg_exp<- c(avg_exp, mean(rexp(40,0.2)))

##To demonstrate the distribution of the average of 40 expinentials we calculate the average of 40 exponentials
## a thousand times and plot the resulting data.  
hist (avg_exp, col="red", main="distribution of averages of 40 exponentials", xlab="average of 40 exponentials",prob=TRUE)
curve(dnorm(x, mean=mean(avg_exp), sd=sd(avg_exp)), add=TRUE)
##we can see that the resulting distribution is approximately normal with a mean of 
mean (avg_exp)
## and a variance of
var(avg_exp)
## Now lets take a look at the exponential distribution 
exp_distrib <- rexp(10000, 0.2)
hist (exp_distrib, main="distribution of 10000 exponentials", xlab="exponentials")
abline(v=mean(exp_distrib), col="blue")



2. Show how variable the sample is (via variance) and compare it to the theoretical variance of the distribution.
3. Show that the distribution is approximately normal.

