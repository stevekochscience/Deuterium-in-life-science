# Code is public domain--Steven J. Koch
# This program is to learn how to include confidence intervals on a set of data with binomial statistics. In our case, measuring seed germination percentage as a function of time


require(binom) #binomial statitistics package see: http://rss.acs.unt.edu/Rdoc/library/binom/html/binom.confint.html 

# may as well use a sub-set of data from Anthony's data: https://docs.google.com/spreadsheet/pub?hl=en_US&hl=en_US&key=0AhLNnjMk2r_qdFJRNGNaN0RNM0RucHE5dUY4Q3BNTFE&single=true&gid=0&range=A1%3AAC12&output=csv

dayNum <- (0:7)
numGermDI <- c(0,0,13,18,20,20,20,20)
maxGermDI <- 20
numGermDDW <- c(0,0,8,9,12,13,14,14)
maxGermDDW <- 16

percentGermDI <- numGermDI / maxGermDI
DIData <- data.frame(dayNum, numGermDI, maxGermDI, percentGermDI)
names(DIData) <- c("DayNumber", "NumberGerminated", "NumberSeeds", "PercentGerminated")

percentGermDDW <- numGermDDW / maxGermDDW
DDWData <- data.frame(dayNum, numGermDDW, maxGermDDW, percentGermDDW)
names(DDWData) <- c("DayNumber", "NumberGerminated", "NumberSeeds", "PercentGerminated")

conflev <- 0.80
bayesDI <- binom.bayes(DIData$NumberGerminated, DIData$NumberSeeds, conf.level = conflev)
bayesDDW <- binom.bayes(DDWData$NumberGerminated, DDWData$NumberSeeds, conf.level = conflev)

#I use following line and dev.off() at bottom to save the graph
png (paste("RC Bayes Conf ", conflev, ".png", sep = "" ))

#at this point, the following code will plot the mean and confidence range from the bayes binomial method.  i don't know how
#to rescale the axes, though, so the scale is set by the first plot() call.  should be easy, I just don't know how
plot(DIData$DayNumber, bayesDI$mean, type="n", xlab = "days", ylab = "ratio germinated")

#I don't really understand multiline titles, but the following is from http://tolstoy.newcastle.edu.au/R/help/05/02/13159.html
title("Repeating Crumley Trial 3", line = 2)
title(paste("DI Water (N=", maxGermDI, "), ", "DDW (N=", maxGermDDW, ")", sep = ""), line = 1)

lines(DIData$DayNumber,bayesDI$mean, type="l", lty=1, col="black")
lines(DIData$DayNumber,bayesDI$lower, type="l", lty=2, col="black")
lines(DIData$DayNumber,bayesDI$upper, type="l", lty=2, col="black")

lines(DIData$DayNumber,bayesDDW$mean, type="o", pch=22, lty=1, col="magenta")
lines(DIData$DayNumber,bayesDDW$lower, type="l", lty=2, col="magenta")
lines(DIData$DayNumber,bayesDDW$upper, type="l", lty=2, col="magenta")

legend("bottomright", c("DI bayes mean", paste("DI bayes conf.", conflev), "DDW bayes mean", paste("DDW bayes conf.", conflev)), lty = c(1, 2, 1, 2), 
       col = c("black", "black", "magenta", "magenta"), inset = 0.1)

dev.off()
