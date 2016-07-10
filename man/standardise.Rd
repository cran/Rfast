\name{Standardisation}
\alias{standardise}
\alias{standardise}
\title{
Standardisation
}

\description{
Standardisation
}

\usage{
standardise(x, center = TRUE, scale = TRUE)
}

\arguments{
\item{x}{
A matrix with data. It has to be matrix, if it is data.frame for example the function does not turn it into a matrix.
}
\item{center}{
Should the data be centred as well? TRUE or FALSE.
}
\item{scale}{
Should the columns have unit variance, yes (TRUE) or no (FALSE)?
}
}

\details{
Similar to R's built in functions "scale" there is the option for centering or scaling only or both (default).
}

\value{
A matrix with the standardised data.
}

%\references{
%Shabalin, A.A. (2012). Matrix eQTL: Ultra fast eQTL analysis via large matrix operations. Bioinformatics 28(10): 1353-1358.
%}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr> and Manos Papadakis <papadakm95@gmail.com>.
}

%\note{
  %%  ~~further notes~~
    %}

\seealso{
\code{\link{colVars}, \link{cova}, \link{cora}
}
}

\examples{
x <-matrix( rnorm( 10000 * 100), ncol = 100 )
system.time( scale(x)[1:10000, ] )
system.time( standardise(x) )
a1 <- standardise(x)
a2 <- scale(x)[1:10000, ]
all.equal(a1, a2)

}

\keyword{ Standardisation }
