importDIMACSAscii<-function(filename = "") {
openFile<-filename

if(openFile == "")
  openFile<-file.choose() #show a pop to get the DIMACS ascii file if not provided

cat("Importing Ascii file: ",openFile,"\n")
output<-.readDIMACSFile(openFile) #create the DIMACS matrix with the file information
cat("File imported\n")
return(output) #return the matrix
}
