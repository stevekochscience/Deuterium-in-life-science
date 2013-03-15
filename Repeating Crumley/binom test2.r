# Code is public domain--Steven J. Koch
# This program is to learn how to include confidence intervals on a set of data with binomial statistics. In our case, measuring seed germination percentage as a function of time


require(binom) #binomial statitistics package see: http://rss.acs.unt.edu/Rdoc/library/binom/html/binom.confint.html 

# may as well use a sub-set of data from Anthony's data: https://docs.google.com/spreadsheet/pub?hl=en_US&hl=en_US&key=0AhLNnjMk2r_qdFJRNGNaN0RNM0RucHE5dUY4Q3BNTFE&single=true&gid=0&range=A1%3AAC12&output=csv

dayNum <- (0:10)
numGermDI <- c(0,1,2,20,70,79,80,81,83, 84, 84)
maxGermDI <- 96
numGermDDW <- c(0,0,10,42,55,58,59,60,60,60,60)
maxGermDDW <- 68
numGerm33 <- c(0,0,7,22,71,80,84,85,88,88,89)
maxGerm33 <- 96
numGerm66 <- c(0,0,0,3,7,28,44,58,64,69,73)
maxGerm66 <- 86
numGerm99 <- c(0,0,0,0,0,0,0,0,0,0,0)
maxGerm99 <- 85

percentGermDI <- numGermDI / maxGermDI
DIData <- data.frame(dayNum, numGermDI, maxGermDI, percentGermDI)
names(DIData) <- c("DayNumber", "NumberGerminated", "NumberSeeds", "PercentGerminated")

percentGermDDW <- numGermDDW / maxGermDDW
DDWData <- data.frame(dayNum, numGermDDW, maxGermDDW, percentGermDDW)
names(DDWData) <- c("DayNumber", "NumberGerminated", "NumberSeeds", "PercentGerminated")

percentGerm33 <- numGerm33 / maxGerm33
Data33 <- data.frame(dayNum, numGerm33, maxGerm33, percentGerm33)
names(Data33) <- c("DayNumber", "NumberGerminated", "NumberSeeds", "PercentGerminated")

percentGerm66 <- numGerm66 / maxGerm66
Data66 <- data.frame(dayNum, numGerm66, maxGerm66, percentGerm66)
names(Data66) <- c("DayNumber", "NumberGerminated", "NumberSeeds", "PercentGerminated")

percentGerm99 <- numGerm99 / maxGerm99
Data99 <- data.frame(dayNum, numGerm99, maxGerm99, percentGerm99)
names(Data99) <- c("DayNumber", "NumberGerminated", "NumberSeeds", "PercentGerminated")

conflev <- 0.95
confDI <- binom.logit(DIData$NumberGerminated, DIData$NumberSeeds, conf.level = conflev)
confDDW <- binom.logit(DDWData$NumberGerminated, DDWData$NumberSeeds, conf.level = conflev)
conf33 <- binom.logit(Data33$NumberGerminated, Data33$NumberSeeds, conf.level = conflev)
conf66 <- binom.logit(Data66$NumberGerminated, Data66$NumberSeeds, conf.level = conflev)
conf99 <- binom.logit(Data99$NumberGerminated, Data99$NumberSeeds, conf.level = conflev)

#I use following line and dev.off() at bottom to save the graph
png (paste("RC Logit Conf ", conflev, ".png", sep = "" ))

#at this point, the following code will plot the mean and confidence range from the bayes binomial method.  i don't know how
#to rescale the axes, though, so the scale is set by the first plot() call.  should be easy, I just don't know how
plot(DIData$DayNumber, confDI$mean, type="n", xlab = "days", ylab = "ratio germinated")

#I don't really understand multiline titles, but the following is from http://tolstoy.newcastle.edu.au/R/help/05/02/13159.html
title("Repeating Crumley Multiple Trials")
#title(paste("DI Water (N=", maxGermDI, "), ", "DDW (N=", maxGermDDW, ")", sep = ""), line = 1)

lines(DIData$DayNumber,confDI$mean, type="l", lty=1, col="black")
lines(DIData$DayNumber,confDI$lower, type="l", lty=2, col="black")
lines(DIData$DayNumber,confDI$upper, type="l", lty=2, col="black")

lines(DIData$DayNumber,confDDW$mean, type="l", lty=1, col="magenta")
lines(DIData$DayNumber,confDDW$lower, type="l", lty=2, col="magenta")
lines(DIData$DayNumber,confDDW$upper, type="l", lty=2, col="magenta")

lines(DIData$DayNumber,conf33$mean, type="l", lty=1, col="green")
lines(DIData$DayNumber,conf33$lower, type="l", lty=2, col="green")
lines(DIData$DayNumber,conf33$upper, type="l", lty=2, col="green")

lines(DIData$DayNumber,conf66$mean, type="l", lty=1, col="orange")
lines(DIData$DayNumber,conf66$lower, type="l", lty=2, col="orange")
lines(DIData$DayNumber,conf66$upper, type="l", lty=2, col="orange")

lines(DIData$DayNumber,conf99$mean, type="l", lty=1, col="cyan")
lines(DIData$DayNumber,conf99$lower, type="l", lty=2, col="cyan")
lines(DIData$DayNumber,conf99$upper, type="l", lty=2, col="cyan")

legend("bottomright", c("DI mean", paste("DI conf.", conflev), "DDW mean", paste("DDW conf.", conflev), "33% mean", 
       paste("33% conf.", conflev),"66% mean", paste("66% conf.", conflev), "99% mean", paste("99% conf.", conflev)), 
       lty = c(1, 2, 1, 2, 1, 2, 1, 2), 
       col = c("black", "black", "magenta", "magenta", "green", "green", "orange", "orange", "cyan", "cyan"), inset = 0.1)

dev.off()
