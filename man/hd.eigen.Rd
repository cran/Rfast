\name{Eigenvalues in high dimensional principal component analysis}
\alias{hd.eigen}
\title{
Eigenvalues in high dimensional principal component analysis
}

\description{
Eigenvalues in high dimensional (n<<p) principal component analysis.
}

\usage{
hd.eigen(x, center = TRUE, scale = FALSE)
}

\arguments{
\item{x}{
A numerical matrix with data where the rows are the observations and the columns are the variables.
}
\item{center}{
Do you want your data centered? TRUE or FALSE.
}
\item{scale}{
Do you want each of your variables scaled, i.e. to have unit variance? TRUE or FALSE.
}
}

\details{
When n<<p, at most the first n eigenvalues are non zero. Hence, there is no need to calculate the other 
p-n zero eigenvalues. When center is TRUE, the eigenvalues of the covariance matrix are calculated. 
When both the center and scale is TRUE the eigenvalues of the correlation matrix are calculated.
}

\value{
A vector with the first n eigenvalues and eigenvectors. 
}

\author{
Michail Tsagris and Giorgos Borboudakis

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr>.
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{prcomp}, \link{eigen}, \link{rmdp}
}
}

\examples{
x <- matrix( rnorm(40 * 100), ncol = 100)
a <- hd.eigen(x)
b <- prcomp(x)
b$sdev^2
x <- NULL
}

\keyword{ Eigenvalues }
\keyword{ high dimensional data }
