\name{gcolor-package}
\Rdversion{1.1}
\alias{gcolor-package}
\alias{gcolor}
\docType{package}
\title{
Finds a valid coloring solution for a graph and import graphs from DIMACS files.
}
\description{
The gcolor package provides functions to find valid coloring solutions for a graph and the ability to import graphs from DIMACS files. 
}
\details{
\tabular{ll}{
Package: \tab gcolor\cr
Type: \tab Package\cr
Version: \tab 1.2\cr
Date: \tab 2009-09-27\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
The use of the package is straightforward. First a valid adjacency matrix must be generated or imported. The test() function can be used to generate a test matrix of size nxn and optimal solution cardinality k. Alternatively the importDIMACSAscii() or importDIMACSBin() functions can be used to import a DIMACS file into a valid adjacency matrix. After an adjacency matrix "a" has been created the ineq(a) function is used to generate a valid coloring of the graph. 
}
\author{
Jeffrey Duffany, Ph.D. and Euripides Rivera Negron

Maintainer: Jeffrey Duffany <jeffduffany@gmail.com>
}
\references{
1. Wikipedia - Inequation page:  http://en.wikipedia.org/wiki/Inequation\cr
2. Duffany, J.L. "Systems of  Inequations", 4th LACCEI Conference, Mayaguez, PR, June 21-23, 2006.\cr
3. Duffany, J.L. "Generalized Decision Function and Gradient Search Technique for NP-Complete Problems", XXXII CLEI Conference, Santiago Chile, August 20-23, 2006.\cr
4. Duffany,  J.L.,  "Statistical  Characterization  of  NP-Complete  Problems", Foundations  of  Computer  Science  Conference, World  Computer  Congress, Las Vegas, Nevada, July 14-17, 2008.\cr
5. Duffany, Jeffrey, "Optimal Solution of Constraint Satisfaction Problems", International Conference on Applied Computer Science, Sharm el Sheik, Egypt, December 29-31, 2008.\cr
6. Duffany,  Jeffrey,  "Equivalence  Class  Subset  Algorithm",  International Conference  on Computer  and  Information Technology,  Tokyo,  Japan, May 27-29, 2009.\cr
}
\seealso{
\code{\link{ineq}}, \code{\link{importDIMACSBin}}, \code{\link{importDIMACSAscii}}, \code{\link{test}}
}
\examples{

\dontrun{solution<-ineq(importDIMACSAscii())}

\dontrun{solution<-ineq(importDIMACSBin())}

solution<-ineq(test(10,3)) #solution is a vector with valid coloring of graph test(10,3)


}
