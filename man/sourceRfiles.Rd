\name{Source many R files}
\alias{sourceRfiles}
\title{
Source many R files
}

\description{
Source many R files.
}

\usage{
sourceRfiles(path,local=FALSE,encode = "UTF-8",print_errors=FALSE)
}

\arguments{
\item{path}{
An full path to the directory where R file are.
}
\item{local}{
TRUE, FALSE or an environment, determining where the parsed expressions are evaluated. FALSE (the default) corresponds to the user's workspace (the global environment) and TRUE to the environment from which source is called.
}
\item{encode}{
Character vector. The encoding(s) to be assumed when file is a character string: see file. A possible value is \"unknown\" when the encoding is guessed: see the \"Encodings\" section.
}
\item{print_errors}{
A boolean value (TRUE/FALSE) for printing the errors, if exists, for every file.
}
}

\details{
Reads many R files and source them.
}

\value{
Returns the files that had produced errors during source.
}

%\references{
%}

\author{
R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}


\seealso{
\code{\link{read.directory}, \link{AddToNamespace} 
}
}

\examples{
# for example: path="C:\\some_file\\R\\" where is R files are
# system.time( a<-sourceRfiles(path) )
}

\keyword{ Namespace file }
\keyword{ Export functions }
