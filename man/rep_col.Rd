\name{ Replicate columns }
\alias{rep_col}
\title{
Replicate columns
}

\description{
Replicate columns.
}

\usage{
rep_col(x,ncols)
}

\arguments{
\item{x}{
A vector with data.
}
\item{ncols}{
Number of new columns.
}
}

%\details{
%
%}

\value{
A matrix where each column is equal to \"x\".
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
x <- runif(10)
all.equal(rep_col(x,10),matrix(x,nrow=length(x),ncol=10))
}

\keyword{ Replicate in columns }