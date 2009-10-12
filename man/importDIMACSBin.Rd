\name{importDIMACSBin}
\Rdversion{1.1}
\alias{importDIMACSBin}
\title{
IMPORT DIMACS GRAPH (BINARY FILE)
}
\description{
Import a DIMACS GRAPH from a DIMACS binary file. Most of the time the file is a .col.b file in DIMACS binary format. 
}
\usage{
importDIMACSBin(filename)
}
\arguments{
	\item{filename}{The DIMACS binary file to be imported into R.  If not provided, the function will request it (in Windows will show a popup).}
}
\details{
The importDIMACSBin will ask you to locate the file to be imported. If the file is in the DIMACS format, a matrix will be returned. The matrix will contains 0 and 1, where 1 are the edges of the DIMACS Graph.
}
\value{
A matrix with the edges read from the DIMACS File.
}
\author{
Euripides Rivera Negron
}
\note{
The file must be a DIMACS format binary file. 
}

\seealso{
\code{\link{importDIMACSAscii}}, \code{\link{matrix}}
}
\examples{

## Import the Graph
\dontrun{dimacMatrix<-importDIMACSBin()}

## Solve the Graph with ineq
\dontrun{solution<-ineq(importDIMACSBin())}
}