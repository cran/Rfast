\name{Hypothesis test for two means of percentages}
\alias{percent.ttest}
\title{
Hypothesis test for two means of percentages
}

\description{
Hypothesis test for two means of percentages.
}

\usage{
percent.ttest(x, y, logged = FALSE)
}

\arguments{
\item{x}{
A numerical vector with the percentages of the first sample. Any value between 0 and 1 
(inclusive) is allowed.
}
\item{y}{
A numerical vector with the percentages of the first sample. Any value between 0 and 1 
(inclusive) is allowed.
} 
\item{logged}{
Should the p-values be returned (FALSE) or their logarithm (TRUE)?
}
}

\details{
This is the \code{\link{prop.reg}} but with a single categorical predictor which has two 
levels only. It is like a t-test for the means of two samples haivng percentages. 
}

\value{
A vector with three elements, the phi parameter, the test statistic and its associated 
p-value.
}

\references{
Papke L. E. & Wooldridge J. (1996). Econometric methods for fractional response 
variables with an application to 401(K) plan participation rates. Journal of Applied 
Econometrics, 11(6): 619-632.

McCullagh, Peter, and John A. Nelder. Generalized linear models. CRC press, USA, 
2nd edition, 1989.  
}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris <mtsagris@uoc.gr> and 
Manos Papadakis <papadakm95@gmail.com>.
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ link{percent.ttests}, \link{prop.reg}, \link{ttest2}, \link{ftest}
}
}

\examples{
x <- rbeta(100, 3, 1)
y <- rbeta(100, 7.5, 2.5)
res<-percent.ttest(x, y)
}

\keyword{ t-test }
\keyword{ percentages }

