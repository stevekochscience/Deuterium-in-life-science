# Code is public domain--Steven J. Koch
# This code looks at Anthony, Alex, and Steven's water FTIR data from December 16, 2011
# http://research.iheartanthony.com/2011/12/16/ftir-study-of-differences-between-d2o-ddw-and-di-water/
# https://docs.google.com/spreadsheet/ccc?key=0Agbdciapt4QZdE95UDFoNHlyNnl6aGlqbGF0cDIzTlE&hl=en_US#gid=0

# RCurl library required to access public google spreadsheet. Use "publish as Webpage" and choose CSV file, sheet 1, whatever range such as A1:B6
# to get the https:// URL from Google that you pass to this
# The following is based on instructions at: http://blog.revolutionanalytics.com/2009/09/how-to-use-a-google-spreadsheet-as-data-in-r.html#
# And suggestion from stackoverflow http://stackoverflow.com/questions/8651062/google-docs-exports-spreadsheet-values-with-commas-read-csv-in-r-treats-these

require(RCurl)
require(binom)

# NOTE:!!! I was having trouble with truncation of significant figures. E.g., 9995.401 was being read in as 10000.
# I think i fixed this by changing formatting display in google docs...however, this introduced commas into my numbers
# and read.csv() treated these numbers as factors. reformatting the spreadsheet as "plain text" solved the problem, but that
# breaks the Google spreadsheet...hmmm
# However, _I think_ to get Google to update publishing, I succeeded by exporting to an Excel spreadsheet. Or maybe just time elapsed.
# Two solutions for the commas were suggested on stackoverflow http://stackoverflow.com/questions/8651062/google-docs-exports-spreadsheet-values-with-commas-read-csv-in-r-treats-these

myCsv <- getURL("https://docs.google.com/spreadsheet/pub?hl=en_US&hl=en_US&key=0Agbdciapt4QZdE95UDFoNHlyNnl6aGlqbGF0cDIzTlE&single=true&gid=0&range=A1%3AG4928&output=csv", ssl.verifypeer=FALSE) 
# ssl.verifypeer=FALSE gets around certificate issues I don't understand.
# The spreadsheet can be viewed at https://docs.google.com/spreadsheet/ccc?key=0Agbdciapt4QZdE95UDFoNHlyNnl6aGlqbGF0cDIzTlE&hl=en_US#gid=0
# The first column is wave# (units?) second column is wavelength, third D2O, fourth DI, fifth DDW, 6th DDW OLD, 7th D2O/DDW mix


fullmatrix <- read.csv(textConnection(myCsv), stringsAsFactors = FALSE)
fullmatrix$wave. <- as.numeric(gsub(",", "", fullmatrix$wave.))
## why is there a . after fullmatrix$wave. ??

fullmatrix$wavelength <- as.numeric(gsub(",", "", fullmatrix$wavelength))

# convert transmission data to absorption coefficient. guessing data = t/t0 * 100 and pathlength of 1 cm
fullmatrix$d2o.abs <- -log(fullmatrix$d2o/100)
fullmatrix$di.abs <- -log(fullmatrix$di/100)
fullmatrix$ddw.abs <- -log(fullmatrix$ddw/100)
fullmatrix$ddw.old.abs <- -log(fullmatrix$ddw.old/100)
fullmatrix$d2o.ddw.mix.abs <- -log(fullmatrix$d2o.ddw.mix/100)
   
#truncate data to wavelength <4000 nm
subdata <- fullmatrix[ which(fullmatrix$wavelength < 4000),]
diffDiDdw <- (subdata$di - subdata$ddw)
diffDdwoldDdw <- (subdata$ddw.old - subdata$ddw)
diffDiD2o <- (subdata$di - subdata$d2o)

# read in the water absorption spectrum (Segelstein 1981) downloaded from http://omlc.ogi.edu/spectra/water/data/segelstein81.dat
# first coloumn is wavelength (nm), second column is absorption (1/cm)
## I changed Dr. Koch's code from read.data to read.csv .. updated file name as well
## I might have been able to read directly from github, but didn't know how
## skip = 4 instead of five, changed the column names in the csv file to the namesin the names(refwater) command to see if it will run
## skip = 4 instead of five, changed the column names to names(refwater) to see if it will run

refwater <- read.csv("segelstein81.csv", header=FALSE, sep="", skip=4)

## commented out next line, I couldn't get it to run with this code
## geting the following error after this line
## Error in names(refwater) <- c("wavelength", "abs") : 
##  'names' attribute [2] must be the same length as the vector [1]
## which is messing up all references to refwater
# names(refwater) <- c("wavelength", "abs")

subref <- refwater[which(refwater$wavelength<5000),] 
#truncate reference data below 5000 nm


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

#Plot DI water compared to Seigelstein 1981
windows()
plot(subdata$wavelength/1000, -log(subdata$di/100), type="n", log="xy", main="Water IR absorption compared with Seigelstein",
     xlab="wavelength (micron)", ylab="absorption (1/cm ?)", ylim=c(0.1,100))
lines(subdata$wavelength/1000, -log(subdata$di/100), type="l", lty=1, col="black")
lines(subref$wavelength/1000, subref$abs, type="l", lty=1, col="red")
## getting an error for subref $ operator is invalid for atomic vectors

legend("bottomright", c("Alex, Anthony DI Water", "Seigelstein 1981"), col=c("black", "red"), lty=1, inset=.1)

#Plot DDW water compared to Seigelstein 1981
windows()
plot(subdata$wavelength/1000, subdata$ddw.abs, type="n", log="xy", main="DDW, DI, D2O IR absorption compared with Seigelstein",
     xlab="wavelength (micron)", ylab="absorption (1/cm ?)", ylim=c(0.1,100))
lines(subdata$wavelength/1000, subdata$ddw.abs, type="l", lty=1, col="black")
lines(subdata$wavelength/1000, subdata$di.abs, type="l", lty=1, col="magenta")
lines(subdata$wavelength/1000, subdata$d2o.abs, type="l", lty=1, col="orange")
lines(subdata$wavelength/1000, subdata$ddw.old.abs, type="l", lty=1, col="cyan")
lines(subdata$wavelength/1000, subdata$d2o.ddw.mix.abs, type="l", lty=1, col="yellow")
lines(subref$wavelength/1000, subref$abs, type="l", lty=1, col="green")
## getting an error for subref $ operator is invalid for atomic vectors


legend("bottomright", c("Alex, Anthony DDW Water", "Alex, Anthony DI Water", "Alex, Anthony D2O Water",
       "Alex, Anthony DDW Old", "Alex, Anthony D2O/DDW mix", "Seigelstein 1981"),
       col=c("black", "magenta", "orange", "cyan", "yellow", "green"), lty=1, inset=.1)
  
                                                              
# this next command is necessary to make the graph save, I don't know why. It makes the graph disappear from the screen though
# I also have trouble viewing the file with external editor unless I close out of R. Obviously doing something wrong.
# dev.off()

