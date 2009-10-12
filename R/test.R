test<-function(n,k)
{
# generates test matrix of dimension = n
# and optimal solution of k equivalence classes
#
size<-n #set size of test matrix to n
if(k>size)#check consistency that k<=n.
k<-size #if k>n set k=n
if(k<2) #if k<2 set k=2
k<-2
k<-round(k,0) #if k not integer round it down to integer value
z<-matrix(0,size,size)#create an nxn matrix z of all zeros
z[1:k,1:k]<-matrix(1,k,k)#embed complete graph dimension k
z[row(z)==col(z)]<-0 #set main diagonal to all zeros
s<-1:k #start creating the optimal solution vector
if(k<size)#there are 2 cases: k=size and k<size
{
for(i in (k+1):size) #if k<size complete s using for loop
s<-c(s,sample(1:k,1)) #finish creating optimal solution vector s
dens<-runif(1) #set the ones density of the matrix z
dens<-max(1/size,dens)#maximum limit on ones density
dens<-min(1,dens)#minimum limits on ones density
for(j in 2:size) #double for loop over upper triangular part of z 
{
for(i in 1:(j-1))#double for loop over upper triangular part of z
{
if((runif(1)<=dens)&&(s[i]!=s[j]))#add constraints up to ones density target
{
z[i,j]<-1 #add constraint to upper triangular part of z
z[j,i]<-1 #add constraint to lower triangular part of z
}}}}
z[row(z)==col(z)]<-0 #reset main diagonal to zero just in case
flag<-1 #run a series of tests to ensure validity of matrix z
for(j in 2:size)#double for loop over upper triangular part of z
{
for(i in 1:(j-1))#double for loop over upper triangular part of z
{
if((z[i,j]==1)&&(s[i]==s[j])) #test consistency of solution vector
flag<-0
if(z[i,j]!=z[j,i]) #test if matrix a is symmetric
flag<-0
}}
if(flag==0)
print("error 1 in test program")
if(length(s)!=size)              #check that length(s) = n
print("error 2 in test program")
if(max(s)!=k)                    #check that max(s) = k
print("error 3 in test program")
if(min(s)!=1)                    #check that min(s)=1 
print("error 4 in test program")
for(i in 1:size)
{
if(z[i,i]!=0)
print("error 5 in test program") #final check that main diagonal = all zeros
#z[i,i]<-s[i] #option to embed solution vector s in main diagonal (disabled)
}
rcperm<-sample(size) #permute vertices so k-clique not in vertices 1:k
z<-z[rcperm,rcperm]  #apply random permutation to matrix z
return(z)
}