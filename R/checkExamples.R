

checkExamples<-function(path.man,dont.read = "",print.errors = FALSE){
  examples_files <- .Call("Rfast_read_examples",PACKAGE = "Rfast",path.man,dont.read)
  error_files<-vector("character")
  examples <- examples_files$examples
  file_names<-examples_files$files
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
  for(i in 1:length(examples)){
    tryCatch(eval(parse(text=examples[i])),error=warning_error_function, warning=warning_error_function)
  }
  error_files
}