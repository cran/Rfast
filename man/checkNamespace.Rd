\name{Check Namespace and Rd files}
\alias{checkNamespace}
\alias{checkAliases}
\alias{checkExamples}
\title{
Check Namespace and Rd files
}

\description{
Check Namespace/Rd and examples files.
}

\usage{
checkNamespace(path.namespace,path.rfolder)
checkAliases(path.man,path.rfolder,dont.read = "")
checkExamples(path.man,dont.read = "",print.errors = FALSE,print.names = FALSE)
}

\arguments{
\item{path.namespace}{
An full path to the "NAMESPACE" file.
}
\item{path.rfolder}{
An full path to the directory that contains the "R" files.
}
\item{path.man}{
An full path to the directory that contains the "Rd" files.
}
\item{dont.read}{
A character vector with the name of the files that you wish not to read. By default it's empty "".
}
\item{print.errors}{
A boolean value (TRUE/FALSE) for printing the errors, if exists, for every file. By default it's "FALSE".
}
\item{print.names}{
A boolean value (TRUE/FALSE) for printing the names of the files before running the examples.
}
}

\details{
For function "checkNamespace": reads from the NAMESPACE file all the export R functions, reads from file R all the R functions and check if all the functions are export.

For function "checkAliases": reads from the man directory all the Rd files, then reads from each file the aliases and check if:
	1) All the R files has man file or an alias.
	2) All aliases belongs to functions.
	3) If there are dublicated aliases.

For function "checkExamples": reads from the man directory all the Rd files, then read from each file the examples and then run each of them. If you want to print the errors then set "print.errors=TRUE" and then you will see all the errors for every file. For succeed run of your code you should first run "library(PACKAGE_NAME)". The argument "print.names" it is very helpful because if any of you function crashes R during running you will never know which one was. So setting it "TRUE", it will print the name of each file before runnign it's example.It might crash, but you will know which file.
}

\value{
For function "checkNamespace": a vector with the names of missing R files.

For function "checkAliases": a list with 3 fields.
\item{Missing Man files}{
A vector with the names of the missing Rd files.
}
\item{Missing R files}{
A vector with the names of the missing R files.
}
\item{Duplicate alias}{
A vector with the names of the dublicate aliases.
}

For function "checkExamples": a list with 2 fields 
\item{Errors}{
A character vector with the names of the Rd files that produced an error.
}
\item{Big Examples}{
A character vector with the names of the Rd files that has big examples per line.
}
}


%\references{
%}

\author{
R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}


\seealso{
\code{\link{read.directory}, \link{AddToNamespace}, \link{sourceR}, \link{sourceRd}, \link{read.examples} 
}
}

\examples{
# for example: path.namespace="C:\\some_file\\NAMESPACE"
# for example: path.rfolder="C:\\some_file\\R\"
# for example: path.man="C:\\some_file\\man\"
# system.time( a<-checkNamespace(path.namespace,path.rfolder) )
# system.time( b<-checkAliases(path.man,path.rfolder) )
# system.time( b<-checkExamples(path.man) )
}

\keyword{ Namespace file }
\keyword{ Checking Alias }
\keyword{ Checking R }
\keyword{ Checking Rd }
\keyword{ Checking Examples }