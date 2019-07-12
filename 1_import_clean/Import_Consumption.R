#Set your working directory
setwd('D:/PhD/R_files')

#Run your user-defined functions and import libraries through other scripts
if(!exists("foo", mode="function")) source("functions.R")
if(!exists("foo", mode="function")) source("Import_Libraries.R")

#Set your working directory
setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/PhD/Data/Consumption')

#Get the file names in your wd
files <- list.files()
#For every textfile name in 'files'
df.list.txt <- lapply(files, importData, 6)

#Name the list
filenames <- paste("ID",basename(file_path_sans_ext(files)), sep="")
names(df.list.txt) <- filenames

#Set your working directory (wd)
setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/PhD/Data/readings/readings')

#Get the file names in your wd
files <-list.files()
#For every textfile name in 'files'
df.list.csv <- lapply(files, importData, 1)
#Name the list
filenames <- paste("ID",basename(file_path_sans_ext(files)), sep="")
names(df.list.csv) <- filenames

#Create 3 columns
df.list.csv <- lapply(df.list.csv, function(x) {if (!is.null(x)) {
  cols <- data.frame(do.call(rbind, strsplit(as.vector(x$V2), split = ",")))
  x <- cbind(x$V1, cols)
  }})
#Name columns
df.list.csv <- lapply(df.list.csv, function(x) {if (!is.null(x)) {
  colnames(x) <- c("V1", "V2", "V3")
  x
}})

#Join the lists
m <- intersect(names(df.list.txt), names(df.list.txt))
df.list <- Map(rbind, df.list.txt[m], df.list.csv[m])