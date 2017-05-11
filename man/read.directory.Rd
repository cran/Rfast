\name{Reading the files of a directory}
\alias{read.directory}
\title{
Reading the files of a directory
}

\description{
Reading the files of a directory.
}

\usage{
read.directory(path.directory)
}

\arguments{
\item{path.directory}{
The full path to the directory. 
For example: "C:\\Users\\username\\Documents\\R\\Manos\\src\\Rfast_1.8.0\R"
}
}

\details{
Takes as an argument a full path to a directory and returns the names of the files.
}

\value{
The names of the files.
}

%\references{
%}

\author{
R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}


\seealso{
\code{\link{AddToNamespace}
}
}

\examples{
# for example: path="C:\\some_file\\"
# call read.directory and it will return 
# you the names of the files inside the directory
# some_file
# system.time( read.directory(path) )
# system.time( list.dirs(path) )
}

\keyword{ Read directory }
