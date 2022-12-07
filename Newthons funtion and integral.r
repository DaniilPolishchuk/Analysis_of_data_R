####newtons method part 2 ####

# f 

f <-  function(x, a, b, d){
  x ^ a + b * log(x + 1) +d
  
}

# derivative of x 
f1 <- function(x, a, b, d){
  a * x^(a - 1) + b/ (x +1)
}

#dx <-  deriv(~ x^ 2,  "x")


# final vertion 

NewtMeth  <- function(f, xinit, f1 = NULL, exp= 1e-7, n = 100, ...){
  x <-  xinit  
  
  i <- 0
  
  repeat{
    if (! is.null(f1)){
      Delta <- f(x, ...)/ f1(x, ...)
    }
    
    else {
       del <- 1e-7
       
       fx <- f(x, ...)
       
       f1x <-  (f(x +del, ...) - fx)/ del
       
       Delta <- fx  / f1x
    }
    
    x <- x - Delta
    
    i  <- i + 1
    
    if(i <= n & abs(Delta) <  exp) {break}
    
    if(i > n){ 
      print("not converged!")
      break
    } 
    
  }# end  of repeat 
  
  return(list (x = x, xinit = xinit, iter = i))
  
}# end of fun

plot(seq(-0.1, 2, length = 100), 
     f(seq(-0.1, 2, length = 100), a = 2, b= -3, d= 1), type = "l")

abline (h = 0, col = "red")
abline (v = c(s1c, s2c), col = c("blue", "violet"))


s1c <-NewtMeth(f, 
               xinit = 0.5,
               f1  = NULL,
               a = 2,
               b = -3,
               d = 1)[1]

s2c <-NewtMeth(f, 
               xinit = 1,
               f1  = f1,
               a = 2,
               b = -3,
               d = 1)[1]

# Example of use of the  function ####
y <- c(9, 14, 3, 3, 8, 7, 7, 6, 7, 0, 6, 0, 5, 1, 3, 12, 2, 4, 0, 11)

l<-  function(theta, y){
  return( sum(y) * log(theta) + length(y) * log(1 -  theta))
}

l1 <- function(theta, y){
  return( sum(y) / theta - length(y) / (1 - theta))
}

l2 <-  function(theta, y){
  return( -sum(y) / theta^2 - length(y) / (1 - theta)^2)
}

theta <-  seq(0.01, 0.99,  by = 0.01)

windows()
plot(theta, l(theta, y), type="l")
abline(v = theta[which.max(l(theta, y))], col = "red")

# which value of theta  the maximizer ? MLE

theta[which.max(l(theta, y))]


#analytical solution 

ml  <- sum(y) / (length(y) + sum(y))

# by using  newmeth

NewtMeth(
  f =  l1,
  xinit = 0.5,
  f1 = l2,
  y = y
)

NewtMeth(
  f =  l1,
  xinit = 0.01,
  y = y
)


# with Poisson rv

lp <- function(theta, y){
  n <- length(y)
  
  return(-  sum(log(factorial(y))) + sum(y) * log(theta) - n  * theta)
  
}

lp1 <- function(theta, y){
  n <- length(y)
  
  return(sum(y) /  (theta -  n))
  
}

lp2<- function(theta, y){
  
  return( - sum(y) /  theta^2)
  
}

# alternative way to obtain the log- likelihood


lp_posson <- function(theta, y){
  return(sum(dpois(y, theta, log = TRUE)))
}

thetas <- seq(1,12, by = .1)

lp_posson(thetas,y) # wrong!!!!!!!!!!!!!

lp(thetas, y) # ok!


sapply(thetas, lp_posson, y= y) #(1)

sapply(thetas, function(x){lp_posson(x,y)}) #(2)

#(1) and (2) are the same 

# emperical way 

theta[which.max(sapply(thetas, function(x){lp_posson(x,y)}))]

mean(y)


###########################################

#data
y <- c(9, 14, 3, 3, 8, 7, 7, 6, 7, 0, 6, 0, 5, 1, 3, 12, 2, 4, 0, 11)

lp1 <- function(theta, y){
  return(sum(y) / theta - length(y))
}

lp2 <- function(theta, y){
  return(- sum(y) / theta ^ 2)
}

thetas <- seq(1,50, by = 0.1)

mean(y)

NewtMeth(
  f = lp1,
  xinit = 11,
  f1 = lp2,
  y = y,
  n = 52
)


lg_posson <- function(theta, y){
  return (sum(dpois(y, theta, log = TRUE)))
}

il1 <- sapply(thetas, lg_posson, y = y)

windows()
plot(thetas, il1)
####

#optimizer 

optimize(f = lg_posson, interval = c(0,150), y = y, maximum = T)

##################
#simple idea of integral and definition of it 
# partition sum of areas of trap

# I = h/2 * [ f(x_0) + f(x_1) + sum_1 to n-1(f(j))]  


# work with new packege
require(ggpubr)


f1 <- function(x){
  return(x ^ 3 * exp(-x) + 0.2)
}

#ugly plot :)
curve(f1(x), from = 0, to = 1)

# let us use the ggplot 

df <- data.frame(cbind(c(0,0,1,1,0), c(0,0.2, f1(1), 0, 0) ) )


g1 <- ggplot()+ ylim(c(0,0.6))+ labs(y = "")+ 
  stat_function(fun = f1, 
                xlim = c(0,1), 
                size = 1.05, 
                alpha = 0.75, 
                col = "blue")+ #use new fun which is the same as curve
  geom_area(stat = "function", 
            data = df, 
            fun = f1, 
            fill = "black", 
            alpha = 0.3, 
            xlim= c(0,1))

# this will be the second plot 


g2 <- ggplot()+ ylim(c(0,0.6))+ labs(y = "")+ 
  stat_function(fun = f1, 
                xlim = c(0,1), 
                size = 1.05, 
                alpha = 0.75, 
                col = "blue")+ 
  geom_area(stat = "function", 
            data = df, 
            fun = f1, 
            fill = "black", 
            alpha = 0.3, 
            xlim= c(0,1))+
  geom_segment(aes(x = 0, y = 0.2, xend=1, yend= f1(1)))+
  geom_segment(aes(x = 1, y = 0, xend=1, yend= f1(1)))+
  geom_segment(aes(x = 0, y = 0, xend=0, yend= 0.2))+
  geom_polygon(data = df, aes(x = X1, y = X2), fill = "blue", alpha = 0.2)

# ggarrange() function which connect to grafs together 
windows()
ggarrange(g1, g2)
  
#with one trapezium is very easy but a very poor approximation 

b <-1; a<- 0
(b-a)/2 * sum(f1(a) + f1(b))

integrate(f = f1, a, b)

# if we use n sub-intervals 

seg <- seq(0,1, length = 20)

fx <- vector(length = length(seg))

#f or each sub-interval calculate the fun.

for (i in 1:length(seg)){
  fx[i] <- f1(seg[i])
}

#create a data frame 

df <- data.frame(xend = seg,
                 y = rep(0,20),
                 yend = fx,
                 yend1 = c(fx[2:20], fx[20]), 
                 xend1 = c(seg[2:20], seg[20])
)

# plot the function 

g3 <- ggplot(data = df)+
  stat_function(fun = f1,size= 1.05, alpha = 0.75, color= "blue")+
  geom_segment(aes(x = xend, y = y, xend = xend, yend = yend))+
  geom_segment(aes(x = xend, y = yend, xend = xend1, yend= yend1))+
  geom_ribbon(aes(x = xend , ymin = y, ymax = yend ), fill = "blue", alpha = 0.3)+
  geom_area(stat = "function", fun = f1, fill = "black", alpha = 0.3, xlim = c(0,1))


windows()

ggarrange(g1, g2, g3)

#####

trapezoid.approx <- function(f, a,b,n){
  
  if (is.function(f)== FALSE){stop("F must be a funtion")}
  
  h <- (b-a)/n
  
  j <- 1:(n-1)
  
  xj <- a + j*h
  
  approx <- h * ( (f(a) + f(b)) / 2  + sum(f(xj)))
  
  return(approx)
  
}

trapezoid.approx(f = f1, 0,1, 10000)

integrate(f1, 0, 1)


f2 <- function(x){
  return( exp(2 * x) *  sin(3 * x))
}

trapezoid.approx(f2, -.5, 2, 10000)

integrate(f2, -.5, 2)


#### final example ####

f3 <- function(x)dnorm(x)

pnorm(2) - pnorm( -1)

integrate(f3, -1,2)

trapezoid.approx(f3, -1,2, 10000)

 

