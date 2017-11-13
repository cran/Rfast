\name{Column and row-wise variances and standard deviations}
\alias{colVars}
\alias{rowVars}
\alias{groupcolVars}
\title{
Column and row-wise variances and standard deviations of a matrix
}

\description{
Column and row-wise variances and standard deviations of a matrix
}

\usage{
colVars(x, suma = NULL, std = FALSE, parallel = FALSE)
rowVars(x, suma = NULL, std = FALSE)
groupcolVars(x, ina, std = FALSE)
}

\arguments{
\item{x}{
A matrix with the data.
}
\item{suma}{
If you already have the column sums vector supply it, otherwise leave it NULL.
}
\item{ina}{
A numerical vector specifying the groups. If you have numerical values, do not put zeros, but 1, 2, 3 and so on. 
}
\item{std}{
A boolean variable specyfying whether you want the variances (FALSE) or the standard deviations (TRUE) of each column.
}
\item{parallel}{
Should parallel implentations take place in C++? The default value is FALSE.
}
}

\details{
We found this in \href{http://stackoverflow.com/questions/25099825/row-wise-variance-of-a-matrix-in-r}{stackoverflow} and was created by \href{http://stackoverflow.com/users/3001626/david-arenburg}{David Arenburg}.
We then modified the function to match the sums type formula of the variance, which is faster.
}

\value{
A vector with the column variances or standard deviations.
}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr> and Manos Papadakis <papadakm95@gmail.com>.
}

\seealso{
\code{ \link{colmeans}, \link{colMedians}, \link{colrange}
}
}

\examples{
x <- matrix( rnorm(100 * 100), ncol = 100 )
a2 <- colVars(x)
groupcolVars( as.matrix(iris[, 1:4]), as.numeric(iris[, 5]) )

x<-a2<-NULL
}

\keyword{ Column-wise variances }
\keyword{ row-wise variances }
\keyword{ variances of many samples }
