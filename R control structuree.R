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
x <- 1
for (i in 2:50) {
  x[i] <- 1 + 1 / x[i-1]
  if(abs(x[i]-x[i-1]) < 1e-10)
    break
}
x[i]

# task 8
n <- 10
leng <- matrix(numeric(n*n), ncol =n)
coords <- matrix(rnorm (2*n), ncol =2)
plot(x = coords[,1], y = coords[,2], type = "p")
i <- 1
j <- 1
for(i in 1:10) {
  for(j in i:10) {
    segments(coords[i,1],coords[i,2],coords[j,1],coords[j,2])
    div1 <- coords[j,1] - coords[i,1]
    div2 <- coords[j,2] - coords[i,2]
    leng[i,j] <- sqrt ( div1 ^ 2 +div2 ^ 2 )
    leng[j,i] <- NA
    if (i>0 & j>i) {
      if (!is.na(leng[i,j-1]) & leng[i,j] < leng[i,j-1]) {
        im <- i
        jm <- j
      } else {}
    } else {}
  }
}
segments(coords[im,1],coords[im,2],coords[jm,1],coords[jm,2],lwd = 4, col = "red")




