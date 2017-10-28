#Assignment 1

load("d:/data/a1.RData")

# 1
densitometry <-  read.csv("d:/data/densitometry.csv", header = TRUE, na.strings = "-")
pima <-  read.table("d:/data/pima.txt", header = TRUE, na.strings = "**")

# 2
# (a) Which lake/loch has the smallest average depth?
lakes[which.min(lakes$MeanDepth), 1]
# Llyn Llygeirian
# (b) Sort the data by the altitude of the lake/loch.
lakes <- lakes[order(lakes$Altitude),]
# (c) Create a new column
lakes$Acidity <- cut(lakes$pH, breaks = c(0, 6.5, 7.5, 14), labels = c("acidic", "neutral", "alkaline"))
# (d) Create a new data frame 
lakes.small <- lakes[lakes$SurfaceArea <= 0.25,]

# 3
# (a)Compute the distribution of statess at time t = 1
T <- matrix(c(0.75, 0.25, 0, 0.25, 0, 0.75, 0, 0.75, 0.25), nrow = 3)
P0 <- matrix(c(0,0,1), nrow = 3)
P1 <- t(T) %*% P0
P1
# P1 =(0, 0.75, 0.25)T
# (b) compute the distribution of states pt for times t = 2, 3, 4, 5.
P2 <- t(T) %*% P1
P3 <- t(T) %*% P2
P4 <- t(T) %*% P3
P5 <- t(T) %*% P4
P2
P3
P4
P5
# P2 = (0.1875 , 0.1875,  0.6250)T
# P3 = (0.187500,  0.515625, 0.296875)T
# P4 = (0.2695312, 0.2695312, 0.4609375)T
# P5 = (0.2695312, 0.4130859, 0.3173828)
# (c) Compute p¡Þ
P <- solve((t(T) + matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)), matrix(c(1, 1, 1), nrow = 3))
# p¡Þ=  0.4609375

# 4
# (a) Add a column
bp$time <- bp$hour * 60 + bp$minute + 1
# (b) Identify for times missing values
bp[is.na(bp$hrt), ]
#   hour minute bps bpd hrt time
#20    0     19 125  72  NA   20
#60    0     59 128  72  NA   60
#66    1      5 127  71  NA   66
#(c)
  #average systolic blood pressure for the first 150 minutes
  mean(bp[bp$time <= 150, 3])
  # 126.1333
  #average diastolic blood pressure for the first 150 minutes
  mean(bp[bp$time <= 150, 4])
  # 71.65333
  #average systolic blood pressure after 150 minutes
  mean(bp[bp$time > 150, 3])
  # 98.53333
  #average diastolic blood pressure after 150 minutes
  mean(bp[bp$time > 150, 4])
  # 55.9
# (d) t-test
x <- bp[bp$time <= 150, 3] 
y <- bp[bp$time > 150, 3]
n1 <- length(x)
n2 <- length(y)
xbar <- mean(x)
ybar <- mean(y)
a <- sum((x - xbar) ^ 2)
b <- sum((y - ybar) ^ 2)
Sxy <- (1 / (n1 + n2 - 2)) * (a + b)
t <- (xbar - ybar) / (sqrt((1/n1 + 1/n2) * Sxy))
t
# t = 23.67698
# test
t.test(x = bp[bp$time <= 150, 3] , y = bp[bp$time > 150, 3] , var.equal = TRUE)
# t = 23.677

# 5
# (a)
i <- 1
sum <- 0
x <- matrix(0, nrow = 3, ncol = 3)
while (i<=3) 
  {
  x[i,i] <- i
  sum <- sum + i
  i <- i+1
  }
x[i-1,i-3] <- sum
x
# (b)
i <- 1
k <- 5
x <- NA
while(i < 5)
{
  j <- 1
  while(j < 4)
  {
    x <- append(x, k, after = length(x))
    k <- k+1
    j <- j+1
  }
  x
  k <- k-4
  i <- i+1
}
x <- x[-is.na(x)]
x

# 6
# (a)
X <- meat[,-101]
Y <- meat[,101]
X <- as.matrix(X)
Y <- as.matrix(Y)
Beta <- solve( t(X) %*% X , t(X) %*% Y)
Beta
# (b)
yh <- X %*% Beta
err <- t(Y - yh) %*% (Y - yh) / length(Y)
err
# err = 0.1323085
# (c)
X2 <- meat2[,-101]
Y2 <- meat2[,101]
X2 <- as.matrix(X2)
Y2 <- as.matrix(Y2)
yh2 <- X2 %*% Beta
err2 <- t(Y2 - yh2) %*% (Y2 - yh2) / length(Y2)
err2
# err = 63.51513
# (d) 1
X <- meat[,-101]
Y <- meat[,101]
X <- as.matrix(X)
Y <- as.matrix(Y)
temp <-  t(X) %*% X + 10^(-8) * diag(100)
Betarr <- solve( temp, t(X) %*% Y)
Betarr
# (d) 2
yh3 <- X %*% Betarr
err3 <- t(Y - yh3) %*% (Y - yh3) / length(Y)
err3
# err = 0.4991867
# (c) 3
X2 <- meat2[,-101]
Y2 <- meat2[,101]
X2 <- as.matrix(X2)
Y2 <- as.matrix(Y2)
yh4 <- X2 %*% Betarr
err4 <- t(Y2 - yh4) %*% (Y2 - yh4) / length(Y2)
err4
# err = 7.547381
# Ridge regression is better suited in meat2. The mean of squared errors
# becomes much more lower than ordinary regression.

# END