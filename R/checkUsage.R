
checkUsage <- function(path.man,path.rfolder,dont.read = "") {
	.Call(Rfast_check_usage,path.man,path.rfolder,dont.read)
}