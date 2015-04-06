################################################################################################################################### 
#This script is written to extract Account holders name on the national KTO (Konekielinen Tiliote standard).                      #
#And to match it with the payment iniating systems records of transactions.                                                       #
#Match between the KTO and the iniating systems records are based on of the reference id and checked further if the names of      #
#the person and account holder match.                                                                                             #
#Names do not need to match 100% but should match within some percentage and non matches should be flagged for further inspection.
#
#KTO spesifications used:https://www.fkl.fi/materiaalipankki/ohjeet/Dokumentit/Konekielinen_tiliote_palvelukuvaus.pdf
#
###################################################################################################################################

###########
#workspace and packages

#require on packages
require("data.table")
require("stringr")
require("stringdist")
require("plyr")


#load packages
library("data.table")
library("stringr")
library("stringdist")
library("plyr")

###########
#Set the directories for the files

#set the directory for the files
ktodir <- as.character("C:/Rprod/kto")

#List KTO files in directory
#files are scanned based on the detection of string "mimlto" in the middle of the filename
kto.files <- list.files(path=ktodir, pattern="[:alnum:]?mimlto.[:alnum:]?")

#for the forloop set first=T
first <- TRUE

#parse.kto function parses the KTO based on the KTO standards
#the following variables are parsed from the KTO:
#name of the account holder,amount of transfer(eurocents), iban account nbr, reference id of the transaction.
#At the current time refid corresponds with the end to end id
#single vector of res is formed containing all the vars.
parse.kto <- function(status) {
  name <- as.character(str_trim(substr(status, 109, 143), side= "both"))
  amount <- as.numeric(str_trim(str_pad(substr(status, 89, 106), width=1, side="left", pad="0"), side= "both"))
  iban <- as.character(str_trim(substr(status, 144, 158), side= "both"))
  refid <- as.character(str_trim(str_pad(substr(status, 159, 179), width=1),side= "both"))
  res <- c(name,amount,iban,refid)
  res[is.na(res)] <- "---"
  res # return
}
empty <-0
#########
#for loop takes the files one at a time and searches for every start of a single transaction
#which starts with rows starting with "T10" and binds every row (appends) of res -vector after the previous loop iteration 

for (f in 1:length(kto.files)){
  temp <- readLines(con = paste(ktodir,kto.files[f], sep="/"))
  for (i in 1:length(temp)) {
                            #characters 99-134 from element
    if (str_detect(temp[i], "^T10")){
                            if(first) {
                              parsed <- parse.kto(temp[i])
                              first <- FALSE
                            }
                            else {
                              parsed <- rbind(parsed,parse.kto(temp[i]))
                            } 
  
                            
    }#counter for empty lines
    else{empty <- empty +1}
  }#resulting structure is a matrix and the column names are set according to the following
  colnames(parsed) <- c("name.x", "amount.x", "account.x", "refid")
}
#create data.table structure from matrix for easier and faster search and assingment
parsed <- as.data.table(as.data.frame(parsed))

#####################################################
#function and for-loop for customer/tranactions data#
#####################################################
#List KTO files in directory
#files are scanned based on the detection of string "transactions" in the middle of the filename
mvj.files <- list.files(path=ktodir, pattern="[:alnum:]?transactions[:alnum:]?")

#for the forloop set first=T
first <- TRUE


#TODO:for loop to read in transaction data from multiple files in ktodir
#and bind them together as a big data.table for merge and comparison
###############################################
for (k in 1:length(mvj.files)){
  if(first){
    cnames <- tolower(names(read.table(paste(ktodir,mvj.files[1], sep="/"),
                               header=TRUE, sep=";", 
                               fileEncoding="UTF-8")))
    
    transactions <- data.table()
    setnames(transactions,, names)
  }
  first <- FALSE
  else{
    temp.transactions <- as.data.table(read.table(paste(ktodir,mvj.files[k], sep="/"),
                                                  header=TRUE, sep="|", 
                                                  fileEncoding="UTF-8"))
    rbind(temp.transactions, transactions,...)  
  }
}#TODO:for-loop for transaction data
###############################################

#read in single file and assing it as a test.set

test.set <- read.table(paste(ktodir,mvj.files[4], sep="/"),fileEncoding="UTF-8", header=FALSE, sep=";", allowEscapes=T)
test.set <- as.data.table(test.set[,-c(1,length(names(test.set)))])
test.set <- as.data.table(test.set)
setnames
###############################################
#for loop function for reading mvj data
temp.transactions <- readLines(con = paste(ktodir,mvj.files[2], sep="/"))
parse.mvj <- function(status.mvj) {
  name <- as.character(str_trim(substr(status, 109, 143), side= "both"))
  amount <- as.numeric(str_trim(str_pad(substr(status, 89, 106), width=1, side="left", pad="0"), side= "both"))
  iban <- as.character(str_trim(substr(status, 144, 158), side= "both"))
  refid <- as.character(str_trim(str_pad(substr(status, 159, 179), width=1),side= "both"))
  res <- c(name,amount,iban,refid)
  res[is.na(res)] <- "---"
  res # return
}

###
test.set <- data.table(scan(paste(ktodir,mvj.files[2], sep="/"),fileEncoding="UTF-8", 
                           sep="|", skip=16, strip.white=TRUE, allowEscapes=FALSE))

x <- fread(paste(ktodir,mvj.files[4], sep="/"),colClasses=c("factor","character","numeric","factor","character","factor","factor"))
setnames(x,names(x),c("pt.id","timestamp","amount.y","rayid","name.y","refid","account.y"))
#is this right ->yes it is
x[,refid_trim:=str_trim(refid),]
#also this
x[,name.y:=str_trim(as.character(name.y)),]
#also this
parsed[,refid_trim:=str_trim(refid),]

x[,refid:=as.factor(refid),]
setkey(x,refid)
setkey(parsed,refid)
#find Ã¤ and substitute with ä
#find <

#use join function on data.table structures (KTO AND test.set) to create a joined table
merged.data <- join(parsed[refid!="",,],test.set[refid!="",,], type="right", by="refid", match="all")

merged.data <- join(parsed[refid!="",,],x[refid!="",,], type="right", by="refid", match="all")
#assign/create osa.match column that calculates Optimal String Alingments for name.x vs. name.y
#Optimal String Assingment is also know as restricted Damerau-Levenshtein distance between two strings
#http://en.wikipedia.org/wiki/Damerauâ€“Levenshtein_distance
#function is from the stringdist package for R and can be found http://cran.r-project.org/web/packages/stringdist/index.html
merged.data[,osa.match:=stringdist(name.x,name.y, method="osa"),by=refid]


#correct the wrong encoding semi-manually
merged.data[, name.y.cor:=str_replace_all(name.y,"Ã¤{1}", "ä"),]
merged.data[, name.y:=str_replace_all(name.y,"[:alpha:]?Ã¶{1}|[:space:]?Ã¶", "ö"),]
