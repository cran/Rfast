\name{Rfast-package}
\alias{Rfast-package}
\docType{package}
\title{
Really fast R functions
}

\description{
A collection of Rfast functions for data analysis. 
Note 1: The vast majority of the functions accept matrices only, not data.frames. 
Note 2: Do not have matrices or vectors with have missing data (i.e NAs). We do no check about them and C++ internally transforms them into zeros (0), so you may get wrong results.
Note 3: In general, make sure you give the correct input, in order to get the correct output. We do no checks and this is one of the many reasons we are fast.   
}

\details{
\tabular{ll}{
Package: \tab Rfast\cr
Type: \tab Package\cr
Version: \tab 1.7.1 \cr
Date: \tab 2016-12-3\cr
License: \tab GPL-2\cr
}
}

\author{
Manos Papadakis <papadakm95@gmail.com>, Michail Tsagris <mtsagris@yahoo.gr>, Ioannis Tsamardinos <tsamard@csd.uoc.gr> , Matteo Fasiolo <matteo.fasiolo@gmail.com>, Giorgos Borboudakis 
<borbudak@gmail.com> and John Burkardt <jburkardt@fsu.edu>
}

\section{Maintainers }{
Manos Papadakis <papadakm95@gmail.com>
}

\note{
Acknowledgments:

We would like to acknowledge Professor Kurt Hornik for his invaluable help with this R package and Erich Studerus for his invaliable comments.
}

\keyword{ Mahalanobis distance }
\keyword{ G^2 test }
\keyword{ variances }
\keyword{ t-tests }
\keyword{ univariate regressions }