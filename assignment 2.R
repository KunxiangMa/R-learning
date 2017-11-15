# Assignment 2
load(url("http://www.stats.gla.ac.uk/~rhaggarty/intro2r/a2.RData"))

# 1
n <- 20
x <- rnorm(n)
d <- numeric(0)# create the vector d
for(i in 1:19)
  d[i] <- x[i+1] - x[i]
d
# 2
# a
plot(x = rc$cal.age, y = rc$rc.age,
     xlab = 'Calibrated Age', ylab = 'Age from Radiocarbon Dating',
     main = 'Radiocarbon Dating')
# b
abline(0, 0.8735)
# c
abline(0, 0.8438, lty = 2)
abline(0, 0.9031, lty = 2)
# d
lowery <- 0.8735 * rc$cal.age - 0.02968 * sqrt(17.50 + rc$cal.age ^ 2)
x <- rc$cal.age
lines(x, lowery, lty=3)
uppery <- 0.8735 * rc$cal.age + 0.02968 * sqrt(17.50 + rc$cal.age ^ 2)
lines(x, uppery, lty=3)
# e
x <- rc$cal.age
temp1 <- rbind(c(min(x),0.8735 * min(x)),c(max(x),0.8438 * max(x)),c(max(x),0.9031 * max(x)))
polygon(temp1,col = 'gray', border = NA)
points(x = rc$cal.age, y = rc$rc.age)
abline(0, 0.8735)

# 3
Gamma <- matrix(c(1,1,1))
A <- matrix(c(3,2,-4,2,0,2,-4,2,3),nrow = 3)
for (i in 1:200) {
  B <- A %*% Gamma
  C <- sqrt(sum(B[,1]^2))
  Gamma <- (1/C) * B
}
Gamma
# check
eigen(A)

# 4
rand <- trunc(runif(100,min = 1,max = 5))
A <- c(0,-1)
B <- c(0,1)
C <- c(-1,0)
D <- c(1,0)
X <- c(0,0)
E <- c(0,0)
for (i in 1:100) {
  if(rand[i] == 1){E <- E+A}
  if(rand[i] == 2){E <- E+B}
  if(rand[i] == 3){E <- E+C}
  if(rand[i] >= 4){E <- E+D}
  X <- rbind(X,E)
}
plot(X, pch = 16)
lines(X)

# 5
x <- c(1,2,7,7,7,4,5,2,4,5,4,4)
a <- 0
max(x)
for (i in 1:length(x)) {
  if(x[i]==max(x))
    a <- a+1
}
a

# 6
library(MASS)
data(mammals)
# a
ord <- rank(mammals$brain/mammals$body)
X <- mammals[ord[1:5],]
X
# b 1
rho.boot <- rep(0,10000)
samp.body <- sample(mammals$body,62,replace = TRUE)
samp.brain <- sample(mammals$brain,62,replace = TRUE)
# b 2
for (i in 1:10000) {
  samp.body <- sample(mammals$body,62,replace = TRUE)
  samp.brain <- sample(mammals$brain,62,replace = TRUE)
  rho.boot[i] <- cor(samp.body,samp.brain)
}
quantile(rho.boot, probs = c(0.025,0.975))

# 7
library("ggplot2")
# Function of drawing leaves
Leaf <- function(i=6){
  X <- seq(0,i*pi,0.01/i)
  Y <- 2*sin(2*X)
  Data <- data.frame(X=X,Y=Y)
  ggplot(data = Data ,aes(x = X, y = Y))+
    geom_line(colour = 'red', lwd = 1, alpha = 0.7)+
    coord_polar()
}
#  Change the number of i to draw different pictures
Leaf(6)

# End
