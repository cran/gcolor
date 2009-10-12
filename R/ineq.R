ineq<-function(a)
{
a1<-a
ra<-nrow(a) #size of matrix a (n) 
s<-1:ra # initial solution vector (1:n)
while(sum(a==0)>ra) #while some off-diagonal zeros remain 
{                   #begin main loop
a2<-a%*%a #square the a matrix: a2=a**2
x<-row(a) #find row i values for each element in matrix a
y<-col(a) #find column j values for each element in matrix a
c<-x<y    #select upper triangular portion of a
w<-a[c]   #select all values in upper triangular portion of a
x<-x[c]   #find corresponding i values
y<-y[c]   #find corresponding j values
z<-a2[c]  #find corresponding a2 values
d<-w==0   #only consider cases where a[i,j]=0
x<-x[d]   #find corresponding i values
y<-y[d]   #find corresponding j values
z<-z[d]   #find corresponding a2 values
e<-rev(order(z)) #need to reverse increasing order to get the maximum 
x<-x[e]   #i values in decreasing order of a2
y<-y[e]   #j values in decreasing order of a2
i<-x[1] #extract i value corresponding to max(a2) 
j<-y[1] #extract j value corresponding to max(a2)
if(i>j) #ensure that i<j for correct update of s
{
tmp<-i  #if i>j reverse i and j
i<-j
j<-tmp
}
u<-a[i,] #select row i of a
v<-a[j,] #select row j of a
uv<-u|v  #bitwise logical or of row i and row j
a[i,]<-uv #replace row i of a
a[,i]<-uv #replace column i of a
a<-a[-j,-j] #remove row j and column j from a
s[s==j]<-i  #begin update solution vector s
s<-s-(1)*(s>j) #complete update of solution vector s
ra<-ra-1 #dimension of matrix a reduced by 1
}        #end of main loop  
a<-a1 #restore a to original input matrix
z<-order(s) #find ordering of solution vector
bdfa<-a[z,z] #put matrix a into block diagonal form
z<-s[z] #z is solution vector of block diagonal form of a
#print(bdfa) #print matrix a in block diagonal form (disabled) 
return(s) #return solution vector s for input matrix a
}
