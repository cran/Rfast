\name{ Insert/remove function names in/from the NAMESPACE file }
\alias{ AddToNamespace }
\alias{ RemoveFromNamespace }
\title{
Insert/remove function names in/from the NAMESPACE file
}

\description{
Insert/remove function names in/from the NAMESPACE file.
}

\usage{
AddToNamespace(path.namespace,path.rfolder,sort = FALSE,dontread = "")
RemoveFromNamespace(path.namespace,files.to.remove)
}

\arguments{
\item{path.namespace}{
An full path to the NAMESPACE file.
}
\item{path.rfolder}{
An full path to the directory the new files to be added are stored.
}
\item{sort}{
An boolean value for sorting the exported functions in file NAMESPACE.
}
\item{files.to.remove}{
An character with the names of the functions to be removed form file NAMESPACE.
}
\item{dontread}{
A character vector with the name of the files (without extension) that you wish not to read. By default it's empty "".
}
}

\details{
AddToNameSpace: Reads the files that are exported in NAMESPACE and the files inside rfolder (where R files are) and insert every file that is not exported. To work properly must each R file to have the same name with the exported function. Also every file must have only one function.

RemoveFromNamespace: Remove every function, from argument "files.to.remove", from NAMESPACE.
}

\value{
AddToNameSpace: Returns the file that added in the export or empty character vector if all the files was inserted.

RemoveFromNamespace: Return the files that could not be removed.
}

\author{
R implementation and documentation: Manos Papadakis <papadakm95@gmail.com>.
}


\seealso{
\code{\link{colShuffle}, \link{colVars}, \link{colmeans}, \link{read.directory}
}
}

\examples{
\dontrun{
# for example: path.namespace="C:\\some_file\\NAMESPACE" where is NAMESPACE file
# path.rfolder="C:\\some_file\\R\\" where is R files are
# system.time( a<-AddToNamespace(path.namespace,path.rfolder) )
# if(length(a)==0){
#	print("all the files are inserted")
# }else{
#	print("The new files that inserted are: \n")
#	a
#  }
# system.time( a<-RemoveFromNamespace(path.namespace,c("a","b")) )
# if(length(a)==0){
#	print("all the files are inserted")
# }else{
#	print("The files thatcould not be deleted are: \n")
#	a
#  }
}
}

\keyword{ Namespace file }
\keyword{ Export functions }
\keyword{ Remove functions }
