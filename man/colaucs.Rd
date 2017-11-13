\name{Many are aunder the curve values}
\alias{colaucs}
\title{
Many are aunder the curve values
}

\description{
Many are aunder the curve values.
}

\usage{
colaucs(group, preds)
}

\arguments{
\item{group}{
A numeric vector with two values, one of which must be strictly 1.  
}
\item{preds}{
A numerical matrix with scores, probabilities or any other measure. 
}
}

\details{
The AUCs are calculated column-wise. 
}

\value{
A vector with length equal to the number of columns of the "preds" argument. The AUC vlaues for each column.
}

%\references{
%}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr> and Manos Papadakis <papadakm95@gmail.com>.
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{ttests}, \link{ttest}, \link{ftests}
}
}

\examples{
## 200 variables, hence 200 F-tests will be performed
x <- matrix( rnorm(100 * 200), ncol = 200 )
ina <- rbinom(100, 1, 0.6)
system.time( colaucs(ina, x) )
a <- colaucs(ina, x) 
x <- NULL
}

\keyword{ Area aunder the curve}
