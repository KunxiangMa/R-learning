# practice 6

# Task 1
?dbinom
dbinom(3, 10, 0.5)
dbinom(x = 3, size = 10, prob = 0.5)
dbinom(4, 1, 0.5) # means to evaluate the p.m.f of the Bi(1,0.5) dixtribution of x = 4

# Task 2
# a
plot(c(-3,3), c(-3,3), type="n")
x <- 0
y <- 0
width <- 4
?rect
rect(x-width/2, y-width/2, x+width/2, y+width/2)
# b
square <- function(a,b,c){
  rect(a-c/2, b-c/2, a+c/2, b+c/2)
}
#c
square(0,0,2)
square(0,0,4)
square(0,0,6)
#d
square <- function(a,b,c,...){
  rect(a-c/2, b-c/2, a+c/2, b+c/2,...)
}
square(0,0,4,col='red')

# Task 3
#a
binomial.coefficient <- function(n, k){
  c <- factorial(n)/(factorial(k) * factorial(n - k))
  return(c)
}
binomial.coefficient(6,3)
#b
binary.entropy <- function(p){
  H <- -p * log(p) - (1 - p) * log(1-p)
  return(H)
}
binary.entropy(0.1)
#c
approx.lbincoef <- function(n, k){
  A <- n * binary.entropy(k/n)
  return(A)
}
approx.lbincoef(2,1)
#d
approx.lbincoef(9000,4000)

# Task 4
#a
qudratic <- function(a,b,c){
  delta <- b ^ 2 - 2 * a * c
  if (delta <0 )
    return(NA)
  else
    if(delta == 0)
      return(-b/(2*a))
    else
      return(c(-(b + sqrt(delta))/(2*a), -(b - sqrt(delta))/(2*a)))
}
qudratic(1,1,1)
#b
qudratic <- function(a,b,c){
  if (a == 0 && b == 0){
    return(NA)
  }else{
    if(a == 0){
      return(-c/b)
    }else{
      delta <- b ^ 2 - 2 * a * c
  if (delta <0 )
    return(vector())
  else
    if(delta == 0)
      return(-b/(2*a))
  else
    return(c(-(b + sqrt(delta))/(2*a), -(b - sqrt(delta))/(2*a)))
    }
  }
}

# Task 5

#confidence interval
n <- 30
theta <- 0.6
alpha <-  0.05
sd.theta <- sqrt(theta * (1 - theta) / n)
ci <- theta + c(-1, 1) * qnorm(1 - alpha / 2) * sd.theta
names(ci) <- c("lower", "upper")
ci
#a
ci.proportion.direct <- function(n, theta, alpha){
  sd.theta <- sqrt(theta * (1 - theta) / n)
  ci <- theta + c(-1, 1) * qnorm(1 - alpha / 2) * sd.theta
  names(ci) <- c("lower", "upper")
  return(ci)
}
ci.proportion.direct(50,0.7,0.05)
#b
ci.proportion.direct <- function(n, theta, alpha = 0.05){
  sd.theta <- sqrt(theta * (1 - theta) / n)
  ci <- theta + c(-1, 1) * qnorm(1 - alpha / 2) * sd.theta
  names(ci) <- c("lower", "upper")
  return(ci)
}
ci.proportion.direct(50,0.7)
#c
ci.proportion.direct <- function(n, theta = 0.5, alpha = 0.05, x = 0){
  if(x != 0)
    theta <- x / n
  sd.theta <- sqrt(theta * (1 - theta) / n)
  ci <- theta + c(-1, 1) * qnorm(1 - alpha / 2) * sd.theta
  names(ci) <- c("lower", "upper")
  return(ci)
}
ci.proportion.direct(20,x = 10)
ci.proportion.direct(20,theta = 0.5)
#d
ci.proportion.direct <- function(n, theta = 0.5, alpha = 0.05, x = 0){
  if(x != 0)
    theta <- x / n
  sd.theta <- sqrt(theta * (1 - theta) / n)
  ci <- theta + c(-1, 1) * qnorm(1 - alpha / 2) * sd.theta
  for(i in 1:2){
    if(ci[i] < 0)
      ci[i] <- 0
    if(ci[i] > 1)
      ci[i] <- 1
  }
  names(ci) <- c("lower", "upper")
  return(ci)
}
ci.proportion.direct(100,x = 99)

#Task 6










