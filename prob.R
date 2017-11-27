x1 <- c(0.729,0.243,0.027,0.001)
x2 <- c(0.064,0.288,0.432,0.216)
dist <- c(0,1,2,3)
cov <- sum(x1*x2*dist^2)-sum(x1*dist)*sum(x2*dist)
Dx1 <- sum(x1*dist^2)-(sum(x1*dist))^2
Dx2 <- sum(x2*dist^2)-(sum(x2*dist))^2
r12 <- cov/sqrt(Dx1*Dx2)
2000*0.04*(1-0.04)
(62.5-80)/sqrt(76.8)
