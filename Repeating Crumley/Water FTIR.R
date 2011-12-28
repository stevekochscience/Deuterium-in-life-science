# Code is public domain--Steven J. Koch
# This code looks at Anthony, Alex, and Steven's water FTIR data from December 16, 2011 
# http://research.iheartanthony.com/2011/12/16/ftir-study-of-differences-between-d2o-ddw-and-di-water/
# https://docs.google.com/spreadsheet/ccc?key=0Agbdciapt4QZdE95UDFoNHlyNnl6aGlqbGF0cDIzTlE&hl=en_US#gid=0

# RCurl library required to access public google spreadsheet.  Use "publish as Webpage" and choose CSV file, sheet 1, whatever range such as A1:B6
# to get the https:// URL from Google that you pass to this
# The following is based on instructions at: http://blog.revolutionanalytics.com/2009/09/how-to-use-a-google-spreadsheet-as-data-in-r.html#
# And suggestion from stackoverflow http://stackoverflow.com/questions/8651062/google-docs-exports-spreadsheet-values-with-commas-read-csv-in-r-treats-these

# --- For method #2 (stackoverflow) dealing with commas from Google Docs
# I am not using this method because I don't fully understand it, but leaving it in for further learning
#library(methods)
#setClass("num.with.commas")
#setAs("character", "num.with.commas", function(from) as.numeric(gsub(",", "", from)))

require(RCurl)
require(binom)         

# NOTE:!!!  I was having trouble with truncation of significant figures.  E.g., 9995.401 was being read in as 10000.
# I think i fixed this by changing formatting display in google docs...however, this introduced commas into my numbers
# and read.csv() treated these numbers as factors.  reformatting the spreadsheet as "plain text" solved the problem, but that
# breaks the Google spreadsheet...hmmm
# However, _I think_ to get Google to update publishing, I succeeded by exporting to an Excel spreadsheet.  Or maybe just time elapsed.
# Two solutions for the commas were suggested on stackoverflow http://stackoverflow.com/questions/8651062/google-docs-exports-spreadsheet-values-with-commas-read-csv-in-r-treats-these

myCsv <- getURL("https://docs.google.com/spreadsheet/pub?hl=en_US&hl=en_US&key=0Agbdciapt4QZdE95UDFoNHlyNnl6aGlqbGF0cDIzTlE&single=true&gid=0&range=A1%3AG4928&output=csv", ssl.verifypeer=FALSE)  #ssl.verifypeer=FALSE gets around certificate issues I don't understand.


fullmatrix <- read.csv(textConnection(myCsv), stringsAsFactors = FALSE)
fullmatrix$wave. <- as.numeric(gsub(",", "", fullmatrix$wave.))
fullmatrix$wavelength <- as.numeric(gsub(",", "", fullmatrix$wavelength))

# --- For method 2 of dealing with commas. i am using the simpler, less cool method #1
#fullmatrix <- read.csv(textConnection(myCsv), colClasses = c(rep("num.with.commas", 2), rep("numeric",5) ))
                                   
subdata <- fullmatrix[ which(fullmatrix$wavelength < 4000),]
diffDiDdw <- (subdata$di - subdata$ddw)
diffDdwoldDdw <- (subdata$ddw.old - subdata$ddw)
diffDiD2o <- (subdata$di - subdata$d2o)

# The spreadsheet can be viewed at https://docs.google.com/spreadsheet/ccc?key=0Agbdciapt4QZdE95UDFoNHlyNnl6aGlqbGF0cDIzTlE&hl=en_US#gid=0
# The first column is wave# (units?) second column is wavelength, third D2O, fourth DI, fifth DDW, 6th DDW OLD, 7th D2O/DDW mix

# read in the water absorption spectrum (Segelstein 1981) downloaded from http://omlc.ogi.edu/spectra/water/data/segelstein81.dat
# first coloumn is wavelength (nm), second column is absorption (1/cm)
refwater <- read.table("segelstein81.dat", header=FALSE, sep="", skip=5)  
names(refwater) <- c("wavelength", "abs")
subref <- refwater[which(refwater$wavelength<5000),]  #truncate reference data below 5000 nm

# Save the graph to a PNG file
# png("Repeating Crumley 3.png")

#set up the plot, but add lines/points individually (type="n" turns off first plot)
#ylim sets y scale to the max and min of both datasets
plot(subdata$wave., diffDiDdw, type="n", main="FTIR Water Trial 2", xlab="wave number (cm-1 ?)", ylab="transmission (units?)",
     ylim = c(min(min(diffDdwoldDdw), min(diffDiDdw)), max(max(diffDiDdw), max(diffDdwoldDdw))))  
lines(subdata$wave., (diffDiDdw), type="l", lty=1, col="black")
lines(subdata$wave., (diffDdwoldDdw), type="l", lty=1, col="red")
legend("topright", c("DI water - DDW", "DDW old - DDW"), col=c("black", "red"), lty=1, inset=.1)

windows() #this opens a new graph window
plot(subdata$wave., (diffDiDdw), type="n", main="FTIR Water Trial 2.2", xlab="wave number (cm-1 ?)", ylab="transmission (units?)",
     ylim = c(min(min(diffDiD2o), min(diffDiDdw)), max(max(diffDiD2o), max(diffDiDdw))))  
lines(subdata$wave., (diffDiDdw), type="l", lty=1, col="black")
lines(subdata$wave., (diffDiD2o), type="l", lty=1, col="red")
legend("topright", c("DI water - DDW", "DI water - D2O"), col=c("black", "red"), lty=1, inset=.1)

windows()
plot(subdata$wavelength/1000, -log(subdata$di/100), type="n", log="xy", main="FTIR DI Water", 
     xlab="wavelength (micron)", ylab="absorption (1/cm ?)", ylim=c(0.1,100))
lines(subdata$wavelength/1000, -log(subdata$di/100), type="l", lty=1, col="black")
lines(subref$wavelength/1000, subref$abs, type="l", lty=1, col="red")
legend("bottomright", c("Alex, Ant DI Water", "Seigelstein 1981"), col=c("black", "red"), lty=1, inset=.1)
                                                              
# this next command is necessary to make the graph save, I don't know why.  It makes the graph disappear from the screen though
# I also have trouble viewing the file with external editor unless I close out of R.  Obviously doing something wrong.
# dev.off()

