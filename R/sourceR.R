
sourceR <- function(path,local=FALSE,encode = "UTF-8",print.errors=FALSE) {
  file_names <- .Call('Rfast_read_directory', PACKAGE = 'Rfast',path)
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
  for(i in 1:length(file_names)){
    tryCatch(source(sprintf("%s%s",path,file_names[i]),local = local, encoding = encode),
             error=warning_error_function, warning=warning_error_function)
  }
  if(length(error_files)==0){
    return("Everything is ok..!")
  }
  error_files
}