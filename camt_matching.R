#This script is written to extract Account holders name on a BankToCustomerStatementV02 (camt.053.001.02 ISO20022 standard).
#And to match it with the payment iniating systems records of transactions.
#Match between the camt. and the iniating systems records are based on the name of the account holder
#and a name given for the account holder.
#Names do not need to match 100% but should match within some percentage and non matches should be flagged for further inspection.

#packages
library("data.table")
library("stringr")
library("XML")
library("stringdist")

#======

#Which matching method to use is argumental,
#especially when trying to match names with some certainty with a high cost on false positive (ie. result=no match when there should be a match)

#Namespaces prove at first to be a stumbling block
#StackOverflows http://stackoverflow.com/questions/3876571/how-can-i-use-xpath-querying-using-rs-xml-library
#proved to be quite usefull with the regard of using namespaces.
#To bypass the use of namespaces one can:
#getNodeSet(doc, "//*[local-name() = 'elementname']")

#Test set without for loop
#============
#Set the directory for the camt. XML documents
camtdir <- as.character("your directory")

#List XML files in directory
files <- list.files(path=camtdir, pattern="[:alnum:]?.xml")
stmtxml <- xmlParse(file=paste(camtdir,files[4], sep="/"))
#The selection of multiple local-name defined element can be checked on 
#http://stackoverflow.com/questions/10813653/xpath-select-node-based-in-a-condition-with-local-name
  #Ntry/NtryDtls/TxDtls/RltdPties/Dbtr/Nm
getNodeSet(stmtxml, "//*[local-name() = 'Dbtr'][*[local-name() = 'Nm']]",fun=xmlValue)
getNodeSet(stmtxml, "//*[local-name() = 'Refs'][*[local-name() = 'EndToEndId']]",fun=xmlValue)
getNodeSet(stmtxml, "//*[local-name() = 'time']",fun=xmlValue)
getNodeSet(stmtxml, "//*[local-name() = 'account']",fun=xmlValue)
#============

#Set the directory for the camt. XML documents
camtdir <- as.character("your directory")

#List XML files in directory
files <- list.files(path=camtdir, pattern="[:alnum:]?.xml")
for (i in 1:length(files))
                          {stmtxml[i] <- xmlParse(file=paste(camtdir,files[i], sep="/"))
                          stmtxml[i]
}


namenode <- getNodeSet(stmtxml, "//*/")
RltdPties/Dbtr/*


statement <- data.table("name"=as.character(c(0)), 
                        "refnbr"=0)


#write the function to read in the .xml file
for (i in 1:length(files)){
  statement[i] <- XML()
    rbind(statement)
}
data.table(read.table())