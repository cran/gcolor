.getDIMACSParameters <- function(a) {
  out<-vector(mode="integer")	#initialize the variable to 0
  for(b in a) { #look for the DIMACS matrix sieze
    c<-unlist(strsplit(b, " ", fixed = TRUE)) #split the string by spaces
    if(c[1] == "p") { #if found a p, get the matrix size
      out[1] <- c[3]  #get the number of vertices
      out[2] <- c[4]  #get the number of edges
      break
    }
  }
  return(out) #return the matrix size and qty of edges in a vector
}

.checkDIMACSMatrix<-function(matriz,matrizSize,paramEdges,fillCounter) {
 error<-0
 errorEdgesAscii<-0
 errorEdgesBin<-0
 if(sum(matriz==t(matriz))!=matrizSize*matrizSize){ #test if matriz is symmetric
  matriz<-matriz+t(matriz) #create the full matrix
  
  if((fillCounter)!=paramEdges){   #check if the parameter and the edges match for ascii files
    errorEdgesAscii<-1
  }

  if((fillCounter*2)!=paramEdges){   #check if the parameter and the edges match for binary files
    errorEdgesBin<-1
  }
  
  if((errorEdgesBin+errorEdgesAscii) == 2) { #the edges doesn't match
    cat("WARNING!!!\n")
    cat(" error in input file - qty edges parameter vs edges do not match - check dimacs format\n")
    cat(" Ascii File should match: parameter edges = ",paramEdges," / edges = ",fillCounter,"\n")
    cat(" Binary File should match: parameter edges = ",paramEdges," / edges * 2 = ",fillCounter*2,"\n")
    error<-1
  }
 }
 
 if(sum(apply(matriz,1,sum)==0)!=0) {
   cat("WARNING!!!\n")
   cat(" ",sum(apply(matriz,1,sum)==0)," vertices have no edges.\n")
   error<-1
 }

 if((sum(matriz==0)+sum(matriz==1))!=matrizSize*matrizSize) { #test if matriz is binary
   cat("WARNING!!!\n")
   cat(" error in input file - check the DIMACS format - matrix was not binary\n") #print error message
   error<-1
 }

 if(sum(matriz==t(matriz))!=matrizSize*matrizSize) { #test if matriz is symmetric
   cat("WARNING!!!\n")
   cat(" error in input file - check the DIMACS format - matrix was not symmetric\n") #print error message
   error<-1
 }

# if(error==1) {  #test if an error was found
#   cat("The graph have 1 or more errors, the solutions of the inequation may not be the correct one.\n")
#   matriz<-matrix(0)  #delete the data from the matrix
# }

 return(matriz)
}

.fillDIMACSMatrixEdges<-function(matriz,a,qtyE,mSize) {
 counter<-0                                  #edges counter
 for(b in a) {                               #run the imported file
  c<-unlist(strsplit(b, " ", fixed = TRUE))  #split the string
  if(c[1] == "e"){                           #if found an e (edge) assign 1 in the coordinates inside the matrix
   counter<-counter+1                        #count number of edges
   matriz[as.numeric(c[2]),as.numeric(c[3])]<-1
  }
 }
 matriz<-.checkDIMACSMatrix(matriz,mSize,qtyE,counter)
 return(matriz)  #return the matrix filled
}

.readDIMACSFile<-function(filename){
conFile<-file(filename, "rt") #connect to the text file
a <- readLines(conFile, n = -1L, ok = TRUE, warn = FALSE) #read the DIMACS text file
matrizProperties<-.getDIMACSParameters(a) #get the dimension of the DIMACS matrix
matrizXY<-as.numeric(matrizProperties[1])
qtyEdges<-as.numeric(matrizProperties[2])
matriz<-matrix(0,matrizXY,matrizXY) #create the DIMACS matrix with 0 as default
matriz<-.fillDIMACSMatrixEdges(matriz,a,qtyEdges,matrizXY) #fill the edges into the DIMACS matrix and verify the qty Edges filled vs the parameter.
close(conFile) #close the connection to the file
  
return(matriz) #return the matrix
}