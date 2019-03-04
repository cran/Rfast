
checkUsage <- function(path.man,path.rfolder) {
	.Call(Rfast_check_usage,path.man,path.rfolder)
}