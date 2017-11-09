# Assignment 2
load(url("http://www.stats.gla.ac.uk/~rhaggarty/intro2r/a2.RData"))

# 1
n <- 20
x <- rnorm(n)
d <- numeric(0)# create the vector d
for(i in 1:19)
  d[i] <- x[i+1] - x[i]

# 2
# a
plot(x = rc$rc.age, y = rc$cal.age,
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
# e not finished
x <- rc$cal.age
upy <- function(x){0.8438 * x}
tmp1 <- curve(upy(x))
loy <- function(x){0.9031 * x}
tmp2 <- curve(loy(x))
polygon(tmp1$x, tmp1$y, col = "grey")
polygon(tmp2$x, tmp2$y, col = "grey")

