\name{Many regression based tests for single sample repeated measures}
\alias{rm.lines}
\alias{rm.anovas}
\title{
Many regression based tests for single sample repeated measures
}

\description{
Many regression based tests for single sample repeated measures.
}

\usage{
rm.lines(y, x, logged = FALSE)
rm.anovas(y, x, logged = FALSE) 
}

\arguments{
\item{y}{
A matrix with the data, where each column refers to a different sample of subjects. 
}
\item{x}{
A numerical vector with time (usually) or the the predictor variable. For example the temperature, or the pressure. See the details for more information. Its length is equal to the time points for example, i.e. it must not have the same length as the number of rows of y.   
}
\item{logged}{
Should the p-values be returned (FALSE) or their logarithm (TRUE)?
}
}

\details{
In order to see whether the repeated measurements are associated with a single covariate, e.g. time we perform many regressions and each time calculate the slope. For each subject, its regression slope with the covariate is calculated. In the end a t-test for the hypothesis that the average slopes is zero is performed. The regression slopes ignore that the measurements are not independent, but note that the slopes are independent, because they come from different subjects. This is a simple, summary statistics based approach found in Davis (2002), yet it can provide satisfactory results. The second approach found in Davis (2002) is to perform an ANOVA type approach.    
}

\value{
A matrix with the test statistic (t-test) and its associated p-value.  
}

\references{
Charles S. Davis (2002). Statistical methods for the analysis of repeated measures. Springer-Verlag, New York. 
}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr> and Manos Papadakis <papadakm95@gmail.com>.
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{rint.regbx}, \link{rint.reg}, \link{varcomps.mle}
}
}

\examples{
y <- c(74.5,81.5,83.6,68.6,73.1,79.4,
75.5,84.6,70.6,87.3,73.0,75.0,
68.9,71.6,55.9,61.9,60.5,61.8,
57.0,61.3,54.1,59.2,56.6,58.8,
78.3,84.9,64.0,62.2,60.1,78.7,
54.0,62.8,63.0,58.0,56.0,51.5,
72.5,68.3,67.8,71.5,65.0,67.7,
80.8,89.9,83.2,83.0,85.7,79.6)
y <- as.matrix(y, byrow = TRUE)
x <- c(-10, 25, 37, 50, 65, 80)
rm.lines(y, x)
rm.anovas(y, x)
}

\keyword{ Repeated measures }
\keyword{ univariate approach }
\keyword{ summary statistics }
\keyword{ regression }  
