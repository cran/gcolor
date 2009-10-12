importDIMACSBin<-function(filename = "") {
dimacsBinFile<-filename
if(dimacsBinFile == "")
  dimacsBinFile<-file.choose() #show a pop to get the DIMACS binary file if not provided

cat("Importing binary file: ",dimacsBinFile,"\n")

write(dimacsBinFile, "fileToConvert.txt", sep = "\n") #create a text file to import the DIMACS binary file name to the library

.C("convertDIMACSFromBin2Ascii") #use the library to convert the binary file to ascii

output<-.readDIMACSFile("output.asc") #read the DIMACS ascii file created from the binary file
cat("File imported\n")
return(output) #return the matrix
}
