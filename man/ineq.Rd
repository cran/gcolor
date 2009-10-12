\name{ineq}
\Rdversion{1.1}
\alias{ineq}
\title{
Solves a system of inequations
}
\description{
Generates a valid colouring of a graph by solving the system of inequations representing the graph. 
}
\usage{
ineq(a)
}
\arguments{
  \item{a}{an n x n matrix of ones and zeros representing a graph}
}
\details{
The algorithm works as follows. The solution vector s is initialized to s=(1,2,...,n). The adjacency matrix A is squared and the maximum value of A^2 determined, ignoring any values corresponding to A(i,j)=1 since these vertices cannot be combined. Out of the set of all (i,j) such that A(i,j)=0 choose an (i,j) pair corresponding to the maximum value of A^2. Update the solution vector s to reflect the (i,j) pair combination. Take the ith row of A as a vector xi and the jth row of A as vector xj and perform the bitwise logical OR operation on the two vectors and assign the result to vector xi. Replace row i and column i of A with xi. Remove row j and column j from the matrix A and recursively call ineq(A) on the reduced A matrix. When there are no more remaining (i,j) pairs then the algorithm terminates, returning the original matrix A with the rows and columns permuted into the block diagonal form corresponding to the solution vector s (or the solution vector s itself). Since the A matrix can be quite large, ineq(A) by default only returns the solution vector s and not the block diagonal form of A. }
\value{
A solution vector with a valid colouring of the graph.
}
\references{
Duffany, J.L. "Systems of Inequations", 4th LACCEI Conference, Mayaguez, PR, June 21-23, 2006. 
}
\author{
Jeffrey Duffany, Ph.D. 
}

\seealso{
\code{\link{importDIMACSAscii}}, \code{\link{importDIMACSBin}}
}
\examples{

## Solve the Graph with ineq
\dontrun{solution<-ineq(importDIMACSAscii())}
}
