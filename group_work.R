# read csv

trump <- read.csv('D:/THINGS IN GLA/[subject]Introduction to R (Level M)/week8/trump.csv')

# use package

library("ggplot2")
library("BSDA")

# divide by source

trumpteam <- trump[trump[,1] == 'iOS',]
trumpreal <- trump[trump[,1] == 'Android',]

# select & combine

select <- function(x){
  x <- as.numeric(x)
  w <- boxplot(x = x)
  tmp1 <- max(w[["stats"]])
  tmp2 <- min(w[["stats"]])
  width <- c(tmp2,tmp1)
  return(width)
}

tmp <- select(trumpreal$nwords)
trumpreal$nwords <- as.numeric(trumpreal$nwords)
trumpreal <- trumpreal[trumpreal$nwords >= tmp[1] & trumpreal$nwords <= tmp[2],]
tmp <- select(trumpteam$nwords)
trumpteam$nwords <- as.numeric(trumpteam$nwords)
trumpteam <- trumpteam[trumpteam$nwords >= tmp[1] & trumpteam$nwords <= tmp[2],]
rm(tmp)

trump <- rbind(trumpreal,trumpteam)

# boxplot

ggplot(data = trump) +
  geom_boxplot(aes(source, nwords))

# barplot

trump$hour <- as.numeric(trump$hour)
trump$day <- as.numeric(trump$day)

ggplot(trump)+
  geom_bar(aes(x = sentiment, fill= source), position="dodge", stat="count")

ggplot(trump)+
  geom_histogram(aes(x = nwords, fill= source), position="dodge")

ggplot(trump)+
  geom_bar(aes(x = dow, fill= source), position="dodge", stat="count")

ggplot(trump)+
  geom_bar(aes(x = contains_url, fill= source), position="dodge", stat="count")

ggplot(trump)+
  geom_histogram(aes(x = hour, fill= source), position="dodge")

ggplot(trump)+
  geom_histogram(aes(x = day, fill= source), position="dodge", stat="count")

# Scatter plot

ggplot(trump) +
  geom_point(aes(x = day , y = hour, color = source ), alpha = '0.7')

ggplot(trump) +
  geom_point(aes(x = day , y = nwords, color = source ), alpha = '0.7')

# test

test.word.U <- wilcox.test(trumpreal$nwords,trumpteam$nwords,exact=FALSE,correct=FALSE)
test.word.z <- z.test(trumpreal$nwords,trumpteam$nwords,alternative="two.side",sigma.x = sd(trumpreal$nwords), sigma.y = sd(trumpteam$nwords))

trumpreal$contains_url <- as.numeric(trumpreal$contains_url) - 59
trumpteam$contains_url <- as.numeric(trumpteam$contains_url) - 59
test.url.z <- z.test(trumpreal$contains_url,trumpteam$contains_url,alternative="two.side",sigma.x = sd(trumpreal$contains_url), sigma.y = sd(trumpteam$contains_url))

trumpreal$sentiment <- as.numeric(trumpreal$sentiment) - 48
trumpteam$sentiment <- as.numeric(trumpteam$sentiment) - 48
test.sen.U <- wilcox.test(trumpreal$sentiment,trumpteam$sentiment,exact=FALSE,correct=FALSE)

trumpreal$dow <- as.numeric(trumpreal$dow) - 43
trumpteam$dow <- as.numeric(trumpteam$dow) - 43
test.dow.U <- wilcox.test(trumpreal$dow,trumpteam$dow,exact=FALSE,correct=FALSE)
test.dow.z <- z.test(trumpreal$dow,trumpteam$dow,alternative="two.side",sigma.x = sd(trumpreal$dow), sigma.y = sd(trumpteam$dow))

trumpreal$hour <- as.numeric(trumpreal$hour)
trumpteam$hour <- as.numeric(trumpteam$hour)
test.hour.U <- wilcox.test(trumpreal$hour,trumpteam$hour,exact=FALSE,correct=FALSE)
test.hour.z <- z.test(trumpreal$hour,trumpteam$hour,alternative="two.side",sigma.x = sd(trumpreal$hour), sigma.y = sd(trumpteam$hour))

trumpreal$day <- as.numeric(trumpreal$day)
trumpteam$day <- as.numeric(trumpteam$day)
test.day.U <- wilcox.test(trumpreal$day,trumpteam$day,exact=FALSE,correct=FALSE)
test.day.z <- z.test(trumpreal$day,trumpteam$day,alternative="two.side",sigma.x = sd(trumpreal$day), sigma.y = sd(trumpteam$day))

# the test shows that the source of twitters have close corelation with words, url, sentiment, day, and hour

# translate the data

distr <- as.matrix(abs(trumpreal$nwords - mean(trumpreal$nwords)))
distt <- as.matrix(abs(trumpteam$nwords - mean(trumpteam$nwords)))
resr <- as.matrix(trumpreal$source)
rest <- as.matrix(trumpteam$source)
regsw <-  cbind(rbind(resr,rest),rbind(distr,distt))
for (i in 1:length(regsw[,1])) {
  if(regsw[i,1]=="Android")
    regsw[i,1] <- 2
  if(regsw[i,1]=="iOS")
    regsw[i,1] <- 1
}

