# Assignment 3
# 2294199m KUNXIANG MA 
# Load file

load(url("http://www.stats.gla.ac.uk/~rhaggarty/intro2r/a3.RData"))

# 1
# 1 a

circle <- function(x,y,r){
  library("MASS")
  t <- seq(from = 0, to = 2 * pi, length.out = 100)
  eqscplot(x, y, type="n", xlim=c(x-2*r, x+2*r),  ylim=c(y-2*r, y+2*r))
  polygon(x = x + r * sin(t), y = y + r * cos(t))
}

# 1 a test

circle(1,1,1)

# 1 b

circle <- function(x,y,r){
  library("MASS")
  t <- seq(from = 0, to = 2 * pi, length.out = 100)
  par(pty = 'm')
  eqscplot(x, y, type="n", xlim=c(min(x)-1.5*r[which.min(x)], max(x)+1.5*r[which.max(x)]),
       ylim=c(min(y)-1.5*r[which.min(y)], max(y)+1.5*r[which.max(y)]))
  for (i in 1:length(x)) {
    polygon(x = x[i] + r[i] * sin(t), y = y[i] + r[i] * cos(t))
  }
}

# 1 b test

circle(c(0,2), c(0,0), c(1, 0.5))

# 1 c

circle <- function(x,y,r,col,a,b){
  library("MASS")
  palette(rainbow(7))
  eqscplot(x, y, type="n", xlim=c(min(x)-1.5*r[which.min(x-r)], max(x)+1.5*r[which.max(x+r)]),
           ylim=c(min(y)-1.5*r[which.min(y-r)], max(y)+1.5*r[which.max(y+r)]),
           xlab = a,ylab = b)
  t <- seq(from = 0, to = 2 * pi, length.out = 100)
  data <- cbind(x,y,r,col)
  data <- data[order(data[,3], decreasing = TRUE),]
  for (i in 1:length(x)) {
    polygon(x = data[i,1] + data[i,3] * sin(t), y = data[i,2] + data[i,3] * cos(t),col = data[i,4])
  }
  leg <- as.factor(names(table(col)))
  legend("bottomright", legend = leg, fill = leg)
}

# 1 c test

circle(health$HealthExpenditure,health$LifeExpectancy,sqrt(health$Population)/1e4,
       health$Region, "Health expenditure in $100's", "Life expectancy")

# 2
# 2 a

compute.median <- function(x){
  y <- numeric(0)
  n <- length(x)
  ifelse(n%%2==0, y <- (x[n/2]+x[n/2+1])/2, y <- x[(n+1)/2])
  return(y)
}

# 2 b

x <- seq(-7,13,2)
compute.median(x)

# 2 c

sign.test <- function(x, eta0 = 0){
  n <- length(x)
  ts <- numeric(1)
  for (i in 1:n) {
    ifelse((x[i]-eta0)>=0,ts <- ts+1, ts <- ts)
  }
  pv <- pbinom(ts, n, 1/2)
  text <- rbind("test statistic","p-value")
  res <- rbind(ts,pv)
  return <- cbind(text,res)
  return(return)
}

# 2 d

x <- seq(-7,13,2)
sign.test(x,2)

# 3

zeller <- function(q, m, y){
  # text of week
  week <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  # compute j
  ifelse(m == 1 | m == 2, j <- floor((y - 1) / 100), j <- floor(y / 100))
  # compute k
  ifelse(m == 1 | m == 2, k <- (y - 1) %% 100, k <- y %% 100)
  # compute n
  n <- ((m + 9) %% 12) + 1
  # compute h
  h <- ((floor(2.6 * n - 0.2) + q + k + floor(k / 4) + floor(j / 4) - 2 * j +6) %% 7) + 1
  # return
  return(week[h])
}

# 3 test

zeller(12,11,2015)

# 4 a

is.prime <- function(x){
  if(x < 4){
    if(x < 2){
      return(FALSE)      
    } else {
      return(TRUE)
    }
  } else {
    div <- seq(2,floor(sqrt(x)),by = 1)
    df <- TRUE
    for(i in 1:length(div)){
      df <- x %% div[i] == 0
      if(df)
        break
    }
  return(!df)
  }
}

x <- matrix(seq(1, 100, by = 1),ncol = 1)
x[apply(x,1,is.prime),]

# 4 b

eratosthenes <- function(n){
  x <- seq(2, n, by = 1)
  div <- 2
  while(div < sqrt(n)){
    df <- TRUE
    for (i in 1:length(x)) {
        df[i] <- ((x[i] %% div) == 0) & (x[i] != div)
    }
    x <- x[!df]
    div <- div + 1
  }
  return(x)
}

eratosthenes(100)

# 5

durbin.watson <- function(x){
  sum1 <- numeric(1)
  sum2 <- numeric(1)
  for (i in 2:length(x)) {
    sum1 <- sum1 + (x[i] - x[i-1]) ^ 2
  }
  for (i in 1:length(x)) {
    sum2 <- sum2 + x[i] ^ 2
  }
  d <- sum1 / sum2
  return(d)
}

# 6 a

emp <- function(x){
  n <- length(x)
  x <- x[order(x, decreasing = FALSE)]
  y <- numeric(1)
  for (i in 1:n) {
    y[i] <- i/n
  }
  res <- cbind(x,y)
  return(res)
}

x <- seq(14,1,by = -1)
plot(emp(x),type = "s")
plot(ecdf(x))

# 6 b

emp <- function(x,plot = FALSE){
  n <- length(x)
  x <- x[order(x, decreasing = FALSE)]
  y <- numeric(1)
  for (i in 1:n) {
    y[i] <- i/n
  }
  res <- cbind(x,y)
  if(plot == TRUE)
    plot(res,type = "s")
  return(res)
}

emp(x,TRUE)

# END