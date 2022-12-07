
#### The Central Limit Theorem ####

# The Central Limit Theorem (CLT) states that the sample mean of identically distributed independent
# random variables is approximately normally distributed if the sample size is large. This is true
# for virtually any distribution! We illustrate the Central Limit Theorem using draws from a Poisson
# distribution.

# We begin recalling that if X ~ Poi(lambda), then E(X) = lambda and Var(X) = lambda, where lambda
# represents the mean number of events. We illustrate the Central Limit Theorem by simulating
# 4000 times an experiment in which we take random draws from a Poisson distribution with 
# lambda  = 35 for three sample sizes: 15, 50, and 800. We calculate
# and store the Z-score for the mean of each sample size. 
# Note: Recall the command matrix(data,nrow=X, ncol=Y) will create a matrix with X rows and Y columns, 
# and fill those spaces with a vector of data.


# Finally, we create three stacked histogram of the Z-scores, one for each sample size, and add the
# density curve from the standard normal distribution to each histogram.
## Displaying the distribution of means


lambdat <- 35
sims <- 4000
m <- c(15,50,800)

E.of.X <- V.of.X <- lambdat
  
z <- matrix(NA, nrow = sims, ncol = length(m))


for (j in 1:length(m)){
  
  for (i in 1:sims){
    
    samp <- rpois(n = m[j], lambda = lambdat)
    
    sample.mean <- mean(samp)
    
    
    z[i, j] <- (sample.mean - E.of.X) / sqrt(V.of.X / m[j])
  }
}

summary(z)

# use standart histogram

hist(z[,1], col = "blue")
hist(z[,2], col = "green", add = T, density = 100)
hist(z[,2], col = "orange", add = T, density = 30)

windows()
par(mfrow = c(3,1))

for (j in 1:ncol(z)){
  
  hist(z[,j], 
       xlim = c(-4,4),
       freq = F,
       ylim = c(0, 0.5),
       breaks = 40,
       ylab = "",
       xlabs = "",
       main = paste("sample size of", m[j])
    )
  
  x <- seq(-4,4, by = 0.1)
  lines (x, dnorm(x), col = "blue")
}


#### Hystograms and boxplot with ggplot 

z.data <- data.frame(
  val = as.vector(z),
  sample.size = as.factor(rep(m, each = sims))
)

require(ggplot2); require(ggpubr)

# the default boxplot function 

boxplot(z, 
        xlab = "Sample size",
        col = 3:5,
        names = m,
        horisontal = F)

ggplot(data = z.data,
       aes(x = val, fill = sample.size))+
  geom_boxplot()+
  coord_flip()


#### histogram

graph1 <- ggplot(z.data, aes(x = val, 
                   y = stat(density),
                   fill = sample.size))+
  labs(x = 'z', y = 'f(z)')+
  geom_histogram(bins = 80)

graph11 <- graph1 + facet_grid(sample.size ~ .)


##### density estimation 

?density
set.seed(3)
x<- rnorm(1300)
hist(x, breaks = 1000)
plot(density(x))


graph2 <- ggplot(z.data, aes(x = val, fill =  sample.size))+
  geom_density(alpha = .2)

# split into 3 dif graphs 
graph22 <- graph2 + facet_grid(sample.size ~ .)


combined.plot <- ggarrange(graph11, 
          graph22,
          ncol = 2,
          common.legend = T,
          legend = "bottom",
          labels = c("Histogram", "Density"))
# write on graph
annotate_figure(
  combined.plot,
  top = text_grob(
    'CLT', color = 'blue', face = 'bold', size = 14
  )
)

require(datasauRus)

?datasaurus_dozen

aggregate(
  datasaurus_dozen$x,
  list(datasaurus_dozen$dataset), mean
)

aggregate(
  datasaurus_dozen$y,
  list(datasaurus_dozen$dataset), mean
)

aggregate(
  datasaurus_dozen$x,
  list(datasaurus_dozen$dataset), sd
)

aggregate(
  datasaurus_dozen$y,
  list(datasaurus_dozen$dataset), sd
)

windows()
ggplot(datasaurus_dozen, aes(x = x, y = y, color = dataset))+
  geom_point()+
  theme_void()+
  theme(legend.position = 'none')+
  facet_wrap(~ dataset, ncol = 3)
