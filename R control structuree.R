# task 1
for (i in 2:5) {
  print(i)
}

# task 2
x <- c(1,2,9)
y <- c(2,6,4)
z <- numeric(3)
for (i in 1:3) {
  z[i] <- x[i] * y[i]
}

# task 3
x <- rnorm(1)
if (x < 0) {
  x <- 0
}
print(x)

# task 4
x <- c(1,2,9)
y <- c(2,6,4)
z <- c(3,5,7)
ifelse(x<4, y, z)

# task 5


