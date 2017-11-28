# Assignment 3
# Load file

load(url("http://www.stats.gla.ac.uk/~rhaggarty/intro2r/a3.RData"))

# 1
# 1 a

library("MASS")

circle <- function(x,y,r){
  t <- seq(from = 0, to = 2 * pi, length.out = 100)
  eqscplot(x, y, type="n", xlim=c(x-2*r, x+2*r),  ylim=c(y-2*r, y+2*r))
  polygon(x = x + r * sin(t), y = y + r * cos(t))
}
circle(1,1,1)
# 1 b

circle <- function(x,y,r){
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

circle <- function(x,y,r,col,xlab){
  palette(rainbow(7))
  eqscplot(x, y, type="n", xlim=c(min(x)-1.5*r[which.min(x-r)], max(x)+1.5*r[which.max(x+r)]),
           ylim=c(min(y)-1.5*r[which.min(y-r)], max(y)+1.5*r[which.max(y+r)]))
  par(xlab = "a",)
  for (i in 1:length(x)) {
    polygon(x = x[i] + r[i] * sin(t), y = y[i] + r[i] * cos(t),col = col[i])
  }
  leg <- as.factor(names(table(col)))
  legend("bottomright", legend = leg, fill = leg )
}

circle(health$HealthExpenditure,health$LifeExpectancy,sqrt(health$Population)/1e4
       ,"health$Region,Health expenditure in $100's", "Life expectancy")
class(health$HealthExpenditure)

