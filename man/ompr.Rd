\name{Orthogonal matching pursuit regression}
\alias{ompr}
\title{
Orthogonal matching pursuit regression
}

\description{
Orthogonal matching pursuit regression.
}

\usage{
ompr(y, x, method = "BIC", tol = 2 )
}

\arguments{
\item{y}{
The response variable, a numeric vector.
}
\item{x}{
A matrix with the data, where the rows denote the samples and the columns are the variables. 
}
\item{method}{
You can choose between the change in the BIC ("BIC"), the adjusted \eqn{R^2} or the SSE ("SSE").
}
\item{tol}{
The tolerance value to terminate the algorithm. This is the change in the criterion value between two 
successive steps. The default value is 2. 
}
}

\value{
A matrix with two columns. The selected variable(s) and the criterion value at every step. 
}

\references{
Mazin Abdulrasool Hameed (2012). Comparative analysis of orthogonal matching pursuit and least angle regression. MSc thesis, Michigan State University.

https://www.google.gr/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&ved=0ahUKEwik9P3Yto7XAhUiCZoKHQ8XDr8QFgglMAA&url=https%3A%2F%2Fd.lib.msu.edu%2Fetd%2F1711%2Fdatastream%2FOBJ%2Fdownload%2FComparative_analysis_of_orthogonal_matching_pursuit_and_least_angle_regression.pdf&usg=AOvVaw2fRcSemcbteyWUiAKYi-8B
}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris <mtsagris@yahoo.gr> 
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{cor.fbed}, \link{cor.fsreg}, \link{correls}, \link{fs.reg}
}
}

\examples{
x <- matrnorm(100, 400)
y <- rnorm(100)
a <- ompr(y, x)
a
x <- NULL
}

