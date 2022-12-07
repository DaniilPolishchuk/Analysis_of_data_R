### The Law of Large Numbers ####

# The Law of Large Numbers states that the sample mean approaches the population mean as
# we increase the sample size. We demonstrate this important law by randomly drawing samples of
# increasing sizes from the normal distribution with a specified mean and standard deviation. 
# According to the Law of Large Numbers, as we increase the sample size, we should see the sample 
# mean and standard deviation approach the specified mean and standard deviation of the distribution.
# 
# We begin by illustrating the law of large numbers with a simple numerical example. Using
# similuation, we start with a sample size of one draw from the normal distribution with a mean
# of 1 and standard deviation of 3. We increase the sample size by an increment of one until we
# reach 1200 draws. For each sample size, we calculate and store the mean and standard deviation.
# Finally, we plot the results.


n.draws <- 1200
means <- sds <- rep(NA, n.draws)

set.seed(3)

x<- rnorm(n.draws, mean = 1, sd = 3)

##### simulation, statistically wrong !!!!!!(for sds[1])

for (i in 1:n.draws){
  means[i] <- mean(x[1:i])
  sds[i] <- sd(x[1:i])
}

#######
means[1] <- x[1]
sds[1] <- NA

for (i in 2:n.draws){
  means[i] <- mean(x[1:i])
  sds[i] <- sd(x[1:i])
}


#### cumsum 

means1 <- cumsum(x) / 1:n.draws

sum(abs(means1 - means))



v <- sqrt((cumsum((x - means[1:n.draws])^2) / 1:n.draws )* 1:n.draws / 0:(n.draws -1 ))
sds1 <- sqrt(cumsum(x^2) / 1:n.draws - (cumsum(x)/1:n.draws)^2) * 1:n.draws / 0:(n.draws -1 )

sum(abs(sds[-1] - sds1[-1]))



##### try to write a (face) dynamic plot 


plot(
  means,
  xlab = "NUmbers of Draws",
  ylim = c(0,4),
  col = "white", #ATTANTION!
  ylab = "Mean and SD"
)

abline(h = c(1,3), col = "red")


for( i in seq(3,n.draws, by = 2)){
  Sys.sleep(0.01)
  lines((i-2):i, means[(i-2):i], col = "blue", lwd = 2)
  
  points( (i-2):i, sds[(i-2): i], col = "orange", lwd = 2 )
}

