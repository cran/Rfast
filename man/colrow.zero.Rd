\name{ Check if any column or row is fill with zeros }
\alias{colrow.zero}
\title{
Check if any column or row is fill with zeros
}

\description{
Check if any column or row is fill with zeros.
}

\usage{
colrow.zero(x)
}

\arguments{
\item{x}{
A vector with data.
}
}

\details{
Check all the column if any has all its elements zeros. If found, return \"TRUE\". Otherwise continues with rows. If columns and rows hasn't any zero vector then return \"FALSE\". Even if it returns \"FALSE\" that doesn't mean the determinant can't be zero. It might be but if check before and found any zero vector then for sure the determinant it'll be zero.
}

\value{
A boolean value, \"TRUE\" if any column OR row is zero. \"FALSE\" otherwise.
}

%\references{

%}

\author{
Manos Papadakis

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}

%\note{
  %%  ~~further notes~~
    %}

\seealso{
\code{\link{rowMins}, \link{rowFalse}, \link{nth}, \link{colrange}, \link{colMedians}, \link{colVars}, \link{sort_mat}, \link{rowTrue}
}
}

\examples{
x <- matrix(runif(10*10),10,10)
colrow.zero(x) 
}

\keyword{ Column-Row wise checking }
\keyword{ Determinant }