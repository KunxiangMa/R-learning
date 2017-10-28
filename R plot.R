
#task 1

library(MASS)
data(crabs)
#a
par(mfrow = c(1, 5))
boxplot(crabs$FL , main = 'crabs-FL')
boxplot(crabs$RW , main = 'crabs-RW')
boxplot(crabs$CL , main = 'crabs-CL')
boxplot(crabs$CW , main = 'crabs-CW')
boxplot(crabs$BD , main = 'crabs-BD')
#b-alpha
crabs$CW.orange <- crabs$CW[crabs$sp == 'O']
crabs$CW.blue <- crabs$CW[crabs$sp == 'B']
par(mfrow = c(1, 2))
boxplot(crabs$CW.orange , main = 'crabs-CW-orange')
boxplot(crabs$CW.blue , main = 'crabs-CW-blue')
#b-beta
par(mfrow = c(2, 3))
boxplot(FL~sp, data = crabs, main = 'crabs-FL~sp')
boxplot(RW~sp, data = crabs, main = 'crabs-RW~sp')
boxplot(CL~sp, data = crabs, main = 'crabs-CL~sp')
boxplot(CW~sp, data = crabs, main = 'crabs-CW~sp')
boxplot(BD~sp, data = crabs, main = 'crabs-BD~sp')
#c
par(mfrow = c(1,1))
plot(crabs$CW , crabs$BD)
#d
plot(crabs$CW , crabs$BD , xlab = 'Carapace width (in mm)' , ylab = 'Body depth (in mm)')
#e
plot(crabs$CW , crabs$BD , xlab = 'Carapace width (in mm)' , ylab = 'Body depth (in mm)',
     col = unclass(crabs$sp) , pch = unclass(crabs$sex))
#f
plot(crabs$CW , crabs$BD , xlab = 'Carapace width (in mm)' , ylab = 'Body depth (in mm)',
     col = c('blue' , 'orange')[unclass(crabs$sp)]
     , pch = unclass(crabs$sex))
#another way of f (ggplot)
ggplot(data = crabs , aes( x = CW, y = BD)) +
 labs(title = "Two Species of Leptograpsus" ,
      x = "Carapace width (in mm)" , y = "Body depth (in mm)") +
 theme(plot.title=element_text(size=20)) +
 geom_point(aes(color = sp, shape = sex),alpha = I(0.6),size = 2) +
 scale_color_manual(values=c("Blue", "Orange"),
                    name  ="Species", breaks=c("B", "O"), labels=c("Blue", "Orange")) +
 scale_shape_discrete(name  ="Sex",
                      breaks=c("F", "M"), labels=c("Female", "Male")) 


            