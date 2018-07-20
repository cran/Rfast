

checkExamples<-function(path.man,each = 1,dont.read = "",print.errors = stderr(),print.names = FALSE){
  examples_files <- .Call(Rfast_read_examples,path.man,dont.read)
  error_files<-vector("character")
  examples <- examples_files$examples
  file_names<-examples_files$files
  if(!is.null(print.errors)){
    warning_error_function <-function(err){
      write(paste(file_names[i],":","\n",err),print.errors)
      error_files <<- c(error_files,file_names[i])
    }
  }else{
    warning_error_function <-function(err){
      error_files <<- c(error_files,file_names[i])
    }
  }
  if(print.names){
    for(i in 1:length(examples)){
      print(file_names[i])
      for(j in 1:each){
      	tryCatch(eval(parse(text=examples[i])),error=warning_error_function, warning=warning_error_function)
      }
    }
  }else{
    for(i in 1:length(examples)){
      for(j in 1:each){
      	tryCatch(eval(parse(text=examples[i])),error=warning_error_function, warning=warning_error_function)
      }
    }
  }
  list("Errors"=error_files,"Big Examples"=examples_files$long_lines)
}