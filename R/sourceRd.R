
sourceRd <- function(path,print.errors=FALSE) {
  file_names <- Rfast::read.directory(path)
  error_files<-vector("character")
  if(print.errors){
    warning_error_function <-function(err){
      write(paste(file_names[i],":","\n",err),stderr())
      error_files <<- c(error_files,file_names[i])
    }
  }else{
    warning_error_function <-function(err){
      error_files <<- c(error_files,file_names[i])
    }
  }
  error<-0
  for(i in 1:length(file_names)){
    error<-tools::checkRd(sprintf("%s%s",path,file_names[i]))
    if(length(error)!=0){
      warning_error_function(error)
    }
  }
  if(length(error_files)==0){
    return("Everything is ok..!")
  }
  return(error_files)
}