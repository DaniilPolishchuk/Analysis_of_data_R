# Exercise 1  ####

# Create a function that given an integer will calculate 
# how many divisors it has (other than 1 and itself). 
# Make the divisors appear by screen.

#the function returns vector in which 
#the first element is amount of divisors(without 1 and n)
# and a sequence of all divisors(without 1 and n) 

divisors <- function(n){
  
  # n <- as.interger(readline("Please put num"))
  
  n <- abs(n)  #  use abs to cover the cases in which n<0
  d <- 2:(n/2) #we can use n/2 instead  of n to optimize computation 
  
  return (c( amount = length(d[n%%d == 0]), divisors = d[n%%d == 0]))
}

divisors(-100)
#############
# cat() is  the same as  print()
#############

# Exercise 2 ####

# Modify the function so that: 
# (a) the default is to use rnorm() to generate 20 random normal numbers, 
# and return the standard deviation; 
# (b) if there are missing values, 
# the mean and standard deviation are calculated for the remaining values.
# (c) add the possibility to replace the missing values
# with the median of the data


meanANDsd <- function() {
  #generate 20 random normal numbers  
  x <- rnorm(20)
  
  #replace missing values with the median  
  x[is.na(x)] = median(x)
  
  #get rid of missing values 
  av <- mean(x, na.rm = TRUE)
  sdev <- sd(x, na.rm = TRUE)
  
  return(c(mean = av, sd = sdev)) 
}

meanANDsd()


# Solution by prof.

meanANDsd1 <- function(x = rnorm(20), return.mean = F) {
  
  if (!return.mean){
    return(sd(x))
  }
  else{
    av <- mean(x)
    sdev <- sd(x)
    
    return(c(mean = av, sd = sdev)) 
  }
  
}

meanANDsd1(return.mean = T)

#(b) ####

meanANDsdb <- function(x = rnorm(20), return.mean = F) {
  
  if (!return.mean){
    return(sd(x, na.rm = T))
  }
  else{
    av <- mean(x, na.rm = TRUE)
    sdev <- sd(x, na.rm = TRUE)
    
    return(c(mean = av, sd = sdev)) 
  }
  
}

meanANDsdb(x = c(runif(19), NA))

# (c) ####

meanANDsdc <- function(x = rnorm(20), return.mean = F, replace = F) {
  
  if(!replace){
    
    if (!return.mean){
      return(sd(x, na.rm = T))
    }
    else{
      av <- mean(x, na.rm = TRUE)
      sdev <- sd(x, na.rm = TRUE)
      
      return(c(mean = av, sd = sdev)) 
    }
  }
  else{
    x[is.na(x)] <- median(x, na.rm = T)
    
    if (!return.mean){
      return(sd(x))
    }
    else{
      av <- mean(x)
      sdev <- sd(x)
      
      return(c(mean = av, sd = sdev)) 
    }
  }
  
  
}

meanANDsdc(x = c(runif(19), NA))





# Exercise 3 ####

# With a user-written function, reproduce the Fibonacci sequence. 
# The latter has many mathematical relationships and 
# has been discovered repeatedly in nature. 
# The sequence is constructed considering the sum of the previous 
# two values, initialized with the values 1 and 1.
# 1,1,2,3,5,8,13,21,34,55,89,144,...
# input: an integer indicating the elements of the sequence to be calculated    
    
#Fibonacci sequence function implementation without recursion 
# It return the sequence up to give number 
fib <- function(n){
  
  f <- c(1,1)
  last <- prev.last <- 1 
  
  while(n >= last & last + prev.last <= n){
    f <- c(f, last + prev.last)
    last <- f[length(f)]
    prev.last <-f[length(f)-1]
  }
  
  return(f)
}

fib(56)


#Fibonacci sequence function implementation with recursion

#It returns n-th element of Fibonacci sequence

fib1 <- function(n){
  if(n == 1) return(1)
  else if(n == 2) return(1)
  else return (fib1(n-1)+fib1(n-2))
}

#This function returns all Fibonacci sequence which contain n elements

fib2 <- function(n){
  f <- c()
  
  for (i in 1:n) {
    f <-c(f, fib1(i))
  }
  
  return (f)
}

fib2(10)




# Exercise 4 ####

# write a function that, taking as input a data.frame, 
# calculate mean and variance for each variable and
# collect the results in a list
# input: a data-frame

#data-frame given by task 
data.1 <- data.frame(matrix(rnorm(1000000), ncol = 100))

meanANDvar <- function(x){
  #create output list 
  result <- list()
  
  for(i in 1:ncol(x)){
    
    result[[i]] = c(mean = mean(x[,i]), var = var(x[,i]))
  }
  return(result)
}

meanANDvar(data.1)


abs <- stat(data.1)

# prof solution 

# Exercise 5

# write a function that, taking as input a vectors of positive numbers,
# calculate the "population" coefficient of variation of the differences from the highest 
# value, namely from (2,5,1,9,4) --> the differences will be given by
# 9-2, 9-5, 9-1, etc...
# pay attention on how R calculates the standard deviation!


cv <- function(x){
  
  # size of given vector 
  n <- length(x) 
  
  # find vector of differences from max value 
  dif <- c()
  for(i in 1:n){
     dif<- c(dif, max(x)- x[i])
  }
  # mean value 
  mu <- mean(dif)
  
  # sd function in r find sd of sample, 
  #and as we need population so we can multiply it by sqrt((n-1)/n) 
  #to get right value 
  sigma <- sd(dif) * sqrt((n-1)/n)
  
  # coefficient of variation 
  cv <- sigma / mu 
  
  return(cv)
}

cv(c(2,5,1,9,4))





# Exercise 6

# compute exponential series
# sum_{n=0,1,...} (x^n)/n!
# up to a prespecified tolerance
# check if it converges to exp(x)



exp.series <-function(n,x){
  # sum_{n=0,1,...} (x^n)/n! is converges to exp(x), 
  #as it is Tailor series of exp(x)
  
  #So we can use exp(x) to get result
  
  #return(exp(x))
  
  #Or use the given series 
  sum <- 0
  for (i in 0:n){
    sum<-sum + (x^i)/factorial(i)
  }
  return(sum)
}

exp.series(50,10)






# sum <- 0
# p <- c()
# for (i in 0:20){
#   sum<-sum+(2.7^i)/factorial(i)
#   p <- c(p, sum)
# }
# plot(0:20,p)
# lines(0:20, p, col="green")
# lines(0:5, y = exp(0:5), col= "red")
# abline(v = 0, col="blue")
# abline(h = 0, col= "blue")
# p
