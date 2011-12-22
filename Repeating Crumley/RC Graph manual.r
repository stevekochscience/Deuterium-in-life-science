# Code is public domain--Steven J. Koch
# This program is an example of how to obtain data from a Google spreadsheet and make an XY plot

# RCurl library required to access public google spreadsheet.  Use "publish as Webpage" and choose CSV file, sheet 1, whatever range such as A1:B6
# to get the https:// URL from Google that you pass to this
# The following is based on instructions at: http://blog.revolutionanalytics.com/2009/09/how-to-use-a-google-spreadsheet-as-data-in-r.html#
require(RCurl)

myCsv <- getURL("https://docs.google.com/spreadsheet/pub?hl=en_US&hl=en_US&key=0AhLNnjMk2r_qdFJRNGNaN0RNM0RucHE5dUY4Q3BNTFE&single=true&gid=0&range=A1%3AAC12&output=csv", ssl.verifypeer=FALSE)  #ssl.verifypeer=FALSE gets around certificate issues I don't understand.

fullmatrix <- read.csv(textConnection(myCsv))

# The example spreadsheet can be viewed at https://docs.google.com/spreadsheet/ccc?key=0AhLNnjMk2r_qdFJRNGNaN0RNM0RucHE5dUY4Q3BNTFE
# For the test file using Repeating Crumley Trial 3, the first day column is 4 (all days are the same), 
# the first y column is 3, and each subsequent y is 4 columns after the preceding.   
daynumber <- fullmatrix[,4]
yDIwater <- fullmatrix[,3]
y33D <- fullmatrix[,7]
y66D <- fullmatrix[,11]
y99D <- fullmatrix[,15]
y33DpDDW <- fullmatrix[,19]
y66DpDDW <- fullmatrix[,23]
yDDW <- fullmatrix[,27]

# The following is a basic plot and legend.  Help from help(legend) and http://www.statmethods.net/advgraphs/axes.html and
# http://www.harding.edu/fmccown/R/ ... I am doing a lot of things manually, such as naming colors twice, that maybe could be more concise

# Save the graph to a PNG file
# png("Repeating Crumley 3.png")

#set up the plot, but add lines/points individually (type="n" turns off first plot)
plot(daynumber, yDIwater, type="n", main="Repeating Crumley Trial 3", xlab="days", ylab="percent germinated")  
lines(daynumber,yDIwater, type="o", pch=22, lty=1, col="black")
lines(daynumber,y33D, type="o", pch=22, lty=1, col="red")
lines(daynumber,y66D, type="o", pch=22, lty=1, col="blue")
lines(daynumber,y99D, type="o", pch=22, lty=1, col="green")
lines(daynumber,y33DpDDW, type="o", pch=22, lty=1, col="cyan")
lines(daynumber,y66DpDDW, type="o", pch=22, lty=1, col="magenta")
lines(daynumber,yDDW, type="o", pch=22, lty=1, col="yellow")
legend("bottomright", c("DI Water", "33% D2O", "66%D2O", "99.9% D2O", "DDW + 33%D2O", "DDW + 66%D2O", "DDW"), col=c("black", "red", "blue", "green", "cyan", "magenta", "yellow"), lty=1, inset=.1)

# this next command is necessary to make the graph save, I don't know why.  It makes the graph disappear from the screen though
# I also have trouble viewing the file with external editor unless I close out of R.  Obviously doing something wrong.
# dev.off()

