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
x <- rnorm(10)
# method 1
ifelse(x < 0, 0, x)
# method 2
for (i in 1:10) {
  if (x[i] < 0 ) {
    x[i] <- 0
  }
}
x
# method 3
x <- 1/2 * (x + abs(x))
x

# task 6
x <- c(1,2,9)
cumsum(x)
# new method
m <- x[1]
for (i in 2:length(x)) {
  m[i] <- m[i-1] + x[i]
}
print(m)

# task 7



