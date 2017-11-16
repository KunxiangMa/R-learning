X <- c(20,19.4,23,20,22,20.2,23.8,24,22.5,20.5)
wilcox.test(x = X,
            mu = 20,
            conf.int = TRUE, conf.level = 0.95)
t.test(x = X,
       mu = 20)
barplot(X-20)

x = 0.55
xi =0
r1 = 0
r2 = 0
r12 = 0
save <- matrix(rep(0,0,40),ncol = 4)
for(i in 1:10){
  r1 <- (1/x)*122-200
  r2 <- -(122/(x^2))
  r12 <- r1/r2
  save[i,1] <- x
  save[i,2] <- r1
  save[i,3] <- r2
  save[i,4] <- r12
  x <- x - r12
}
save <- round(save,5)
