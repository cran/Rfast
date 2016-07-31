\name{Index of the columns of a data.frame which are factor variables}
\alias{which_isFactor}
\title{
Index of the columns of a data.frame which are factor variables
}

\description{
Index of the columns of a data.frame which are factor variables.
}

\usage{
which_isFactor(x)
}

\arguments{
\item{x}{
A data.frame where some columns are expected to be factor variables.
}
}

\details{
The function is written in C++ and this is why it is very fast. 
}

\value{
A vector with the column indices which are factor variables. If there are no factor variables it will return an empty vector. 
}

%\references{
%Draper, N.R. and Smith H. (1988). Applied regression analysis. New York, Wiley, 3rd edition.
%}

\author{
Manos Papadakis <papadakm95@gmail.com>

R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{nth}
}
}

\examples{
which_isFactor(iris)
}

\keyword{ Factor variables}
