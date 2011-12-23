# Code is public domain--Steven J. Koch
# This code looks at Anthony, Alex, and Steven's water FTIR data from December 16, 2011 
# http://research.iheartanthony.com/2011/12/16/ftir-study-of-differences-between-d2o-ddw-and-di-water/
# https://docs.google.com/spreadsheet/ccc?key=0Agbdciapt4QZdE95UDFoNHlyNnl6aGlqbGF0cDIzTlE&hl=en_US#gid=0

# RCurl library required to access public google spreadsheet.  Use "publish as Webpage" and choose CSV file, sheet 1, whatever range such as A1:B6
# to get the https:// URL from Google that you pass to this
# The following is based on instructions at: http://blog.revolutionanalytics.com/2009/09/how-to-use-a-google-spreadsheet-as-data-in-r.html#
require(RCurl)
require(binom)

# NOTE:!!!  I am having trouble with truncation of significant figures.  E.g., 9995.401 is being read in as 10000.
myCsv <- getURL("https://docs.google.com/spreadsheet/pub?hl=en_US&hl=en_US&key=0Agbdciapt4QZdE95UDFoNHlyNnl6aGlqbGF0cDIzTlE&single=true&gid=0&range=A1%3AG4928&output=csv", ssl.verifypeer=FALSE)  #ssl.verifypeer=FALSE gets around certificate issues I don't understand.

fullmatrix <- read.csv(textConnection(myCsv))
subdata <- fullmatrix[ which(fullmatrix$wavelength < 4000),]
# The spreadsheet can be viewed at https://docs.google.com/spreadsheet/ccc?key=0Agbdciapt4QZdE95UDFoNHlyNnl6aGlqbGF0cDIzTlE&hl=en_US#gid=0
# The first column is wave# (units?) second column is wavelength, third D2O, fourth DI, fifth DDW, 6th DDW OLD, 7th D2O/DDW mix
  
# Save the graph to a PNG file
# png("Repeating Crumley 3.png")

#set up the plot, but add lines/points individually (type="n" turns off first plot)
#plot(fullmatrix$wavelength, fullmatrix$d2o, type="n", main="FTIR Water Trial 2", xlab="wavelength", ylab="transmission?")  
#lines(fullmatrix$wavelength, fullmatrix$d2o, type="o", lty=1, col="black")
#lines(fullmatrix$wavelength, fullmatrix$di, type="o", lty=1, col="red")

plot(fullmatrix$wavelength, (fullmatrix$ddw-fullmatrix$di), type="n", main="FTIR Water Trial 2", xlab="wavelength", ylab="transmission?")  
lines(fullmatrix$wavelength, (fullmatrix$ddw-fullmatrix$di), type="o", lty=1, col="black")


#legend("bottomright", c("DI Water", "33% D2O", "66%D2O", "99.9% D2O", "DDW + 33%D2O", "DDW + 66%D2O", "DDW"), col=c("black", "red", "blue", "green", "cyan", "magenta", "yellow"), lty=1, inset=.1)

# this next command is necessary to make the graph save, I don't know why.  It makes the graph disappear from the screen though
# I also have trouble viewing the file with external editor unless I close out of R.  Obviously doing something wrong.
# dev.off()

