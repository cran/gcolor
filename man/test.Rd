\name{test}
\Rdversion{1.1}
\alias{test}
\title{
Generates n x n test matrix 
}
\description{
Generates an n x n matrix of optimal solution cardinality k to test the Inequation Algorithm
}
\usage{
test(n,k)
}
\arguments{
  \item{n}{The size of the matrix.}
  \item{k}{The optimal solution cardinality of the coloring of the graph represented by the matrix.}
}
\details{
The test program first picks a random ones density for the A matrix where the ones density is the number of 1 elements in the A matrix divided by the maximum number of ones (n^2 - n). Then it generates a graph with optimal coloring solution cardinaility = k.
}
\value{
A matrix of size n x n and optimal coloring solution cardinality = k
}
\author{
Jeffrey Duffany, Ph.D. 
}

\seealso{
\code{\link{importDIMACSBin}}, \code{\link{importDIMACSAscii}}, \code{\link{ineq}}
}
\examples{

## Creates an n x n matrix of optimal solution cardinality 3
a<-test(10,3)
}
