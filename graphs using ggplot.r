set.seed(1)

# gauseian pop
mu <- 10
sigma2 <-  3

n <-10

#obtain the sample 

x <- rnorm (n, mu, sqrt(sigma2))


# confidence level (1-alpha)
alpha <- 0.05

# n is small and we don know sigma^2 lets us use a t-distibution 

t.alpha.half <- qt(1 - alpha/2, n-1)

xbar <- mean(x)
s2.tilde <- var(x)

LL <- xbar - t.alpha.half * sqrt(s2.tilde / n)
UL <- xbar + t.alpha.half * sqrt(s2.tilde / n) 

cbind(LL, xbar, UL)

# a simulation study : sasmpling distribution of the CI 
# number of samples draw form the sample space 

n.draw <- 100

# prepare the simulation 

sample.means <- rep(NA, n.draw)
sample.variances <- lower.limits <- upper.limits <- rep(NA, n.draw)


# obtain the CIs 
for (i in 1:n.draw){
  x <- rnorm(n, mean = mu, sd = sqrt(sigma2))
  sample.means[i]<- mean(x)
  sample.variances[i] <- var(x)
  
  lower.limits[i] <- sample.means[i] - t.alpha.half * sqrt (sample.variances[i]/n)
  
  upper.limits[i] <-  sample.means[i] + t.alpha.half * sqrt(sample.variances[i]/n) 
}

# compute the coverage probability via simulations 
cover <- ifelse(
  lower.limits < mu & upper.limits > mu,
  1,
  0
)

mean(cover)



# lets us check it grafically 

cols <- c("blue", "green")

plot(
  1:n.draw,
  sample.means,
  ylim = c(min(lower.limits), 
           max(upper.limits)),
  pch = 19
)

segments(1:n.draw, lower.limits, 
         1:n.draw, upper.limits,
         col = cols[cover + 1])

abline(h = mu, col = "red")

#require(ggplot2)


# use ggplot2

# prepare data 
# we need a data frame  

df <- data.frame (
  iter = 1:n.draw,
  means = sample.means,
  LB = lower.limits,
  UB = upper.limits, 
  cover  = as.factor(cover)
)

b1 <- ggplot(data = df, aes(x = iter, 
                      y = means,
                      color = cover,
                      shape = cover))+ 
  geom_point() + 
  labs(x = "Experements", y = "Confidance level") +
  scale_color_discrete(name= "coverage", labels = c("NO", "YES"))+
  scale_shape_discrete(name= "coverage", labels = c("NO", "YES"))+
  geom_hline(aes(yintercept = mu), color = "blue", linetype= "dashed")+
  ggtitle( paste("Coverege of ", mean(cover)))


b1 <- b1 + geom_segment(
  aes(x = iter, 
      y = LB,
      xend = iter,
      yend = UB)
)

windows()
b1


# sampling a gamma distibution 
# example of gamma rv with shape = 5 and scale 1 


xg <- seq(0.1, 20, by=0.1)
# 1/(e^(-x/s))
yg <- dgamma (xg, shape= 5, scale=1) 

plot(xg, yg, type="l", 
     main= "Gamma distibution", 
    ylab = "Density", 
    xlab = "")

lines (xg, dexp(xg, rate = 1), col= "red")

#what if we have a gamma distr data 
# we will use shape = 0.1
# parametrs of the REAL ditribution 

# y ~ gamma (alpha, lambda)
#  E[x] = alpha/lambda

alpha.gamma <- .1
beta.gamma <-  1


mu.gamma <- alpha.gamma/beta.gamma

set.seed(3)

# sample size
n<- 30

alpha <- 0.05

# since n is big enought we use the norm approx

z.alpha.half <- qnorm(1 - alpha/2)

# num of rep of the experiment 

n.draw <- 100

#empty boxes
sample.means <- sample.variances <- lower.limits <- upper.limits <- rep(NA, n.draw)

for (i in 1:n.draw){
  x <- rgamma (n, alpha.gamma, beta.gamma)
  
  sample.means[i]<- mean(x)
  sample.variances[i] <- var(x)
  
  lower.limits[i] <- sample.means[i] - z.alpha.half * sqrt (sample.variances[i]/n)
  
  upper.limits[i] <-  sample.means[i] + z.alpha.half * sqrt(sample.variances[i]/n) 
}



# empirical coverage 

cover.g <- ifelse(
  lower.limits < mu.gamma & upper.limits > mu.gamma,
  1,
  0
)

mean(cover.g)


cols <- c("chocolate", "blueviolet")

windows()
plot (
  1: n.draw,
  sample.means,
  ylim = c (min(lower.limits), max(upper.limits)),
  phc = 18
)
segments(
  1:n.draw,
  lower.limits,
  1: n.draw,
  upper.limits,
  col = cols[cover.g +  1]
)

abline(h = mu.gamma, col= "darkgreen", lwd = 2)


require(ggplot2)

df1 <-  data.frame(
  iter = 1:n.draw,
  means = sample.means,
  LB = lower.limits,
  UB = upper.limits,
  cover = as.factor(cover.g)
)
# ggplot(df1, aes(x = iter,
#                 y = means,
#                 color = cover,
#                 shape =cover)) + geom_point()

g1 <- ggplot(df1, aes(x = iter,
                y = means,
                color = cover,
                shape =cover)) + geom_point()+
  labs(x = "Experements", y= "Confidance level")+
  scale_color_discrete(name = "coverage", labels = c("NO", "YES"))+
  scale_shape_discrete(name= "coverage", labels = c("NO", "YES"))+
  geom_hline(aes(yintercept = mu.gamma), color= "blue", linetype = "dashed")+
  ggtitle( paste("Coverege of ", mean(cover.g)))


g1 <- g1 + geom_segment(
  aes(x = iter, y = LB, xend= iter, yend = UB, color = cover)) 
)