\name{Check Namespace and Rd files}
\alias{checkNamespace}
\alias{checkAliases}
\alias{checkExamples}
\alias{checkTF}
\alias{checkUsage}
\title{
Check Namespace and Rd files
}

\description{
Check Namespace/Rd and examples files.
}

\usage{
checkNamespace(path.namespace,path.rfolder)
checkAliases(path.man,path.rfolder,dont.read = "")
checkTF(path.man,dont.read = "")
checkExamples(path.man,each = 1,dont.read = "",print.errors = stderr(),
	print.names = FALSE)
checkUsage(path.man,path.rfolder,dont.read = "")
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
\item{each}{
An integer value for running \bold{each} example.
}
\item{dont.read}{
A character vector with the name of the files that you wish not to read. By default it's empty "".
}
\item{print.errors}{
Print the errors to a file. By default it's "stdeer()".
}
\item{print.names}{
A boolean value (TRUE/FALSE) for printing the names of the files before running the examples.
}
}

\details{
For function "checkNamespace": reads from the NAMESPACE folder all the export R functions, reads from folder R all the R functions and check if all the functions are export.

For function "checkAliases": reads from the man directory all the Rd files, then reads from each file the aliases and check if:
	1) All the R files has man file or an alias.
	2) All aliases belongs to functions.
	3) If there are dublicated aliases.

For function "checkExamples": reads from the man directory all the Rd files, then read from each file the examples and then run each of them. If you want to print the errors in any file then set "print.errors=file_name" or in the standard error "print.errors=stderr()" and then you will see all the errors for every file. For succeed run of your code you should first run "library(PACKAGE_NAME)". The argument "print.names" it is very helpful because if any of you function crashes R during running you will never know which one was. So setting it "TRUE", it will print the name of each file before runnign it's example.It might crash, but you will know which file. \bold{Remember that there always an error timeout so it might didn't crash the current file but one from the previous.}

For function checkTF: reads from the man directory all the Rd files, then read from each file the examples and checks if any examples has the values "T" and "F" instead "TRUE" and "FALSE". The "T","F" is wrong.

For function checkUsage: reads from the man directory all the Rd files and for each man check if the usage section has the right signature for the functions from the R directory.
This functions has 2 limitations: 
	1) each function should be in one R file with name function_name.R and only this function can be inside the file.
	2) if you want multiple functions inside this file then the main function should be the first on top.
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

For function "checkTF": a list with 2 fields 
\item{TRUE}{
A character vector with the names of the Rd files that has "T".
}
\item{FALSE}{
A character vector with the names of the Rd files that has "F".
}

For function "checkUsage": a list with 2 fields 
\item{missing functions}{
A character vector with the name of the file that is missing and the Rd file that is found.
}
\item{missmatch functions}{
A character vector with the name of the file that has missmatch function and the Rd file that is found.
}
}

\author{
R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}


\seealso{
\code{\link{read.directory}, \link{AddToNamespace}, \link{sourceR}, \link{sourceRd}, \link{read.examples} 
}
}

\examples{
\dontrun{
# for example: path.namespace="C:\\some_file\\NAMESPACE"
# for example: path.rfolder="C:\\some_file\\R\"
# for example: path.man="C:\\some_file\\man\"
# system.time( a<-checkNamespace(path.namespace,path.rfolder) )
# system.time( b<-checkAliases(path.man,path.rfolder) )
# system.time( b<-checkExamples(path.man) )
# system.time( b<-checkExamples(path.man,2) )
# system.time( b<-checkTF(path.man) )
# system.time( b<-checkTF(path.man,path.rfolder) )
}
}

\keyword{ Namespace file }
\keyword{ Checking Alias }
\keyword{ Checking R }
\keyword{ Checking Rd }
\keyword{ Checking Examples }
\keyword{ Checking for TRUE,FALSE }
\keyword{ Checking Usage section }