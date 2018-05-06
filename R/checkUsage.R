
checkUsage <- function(path.man,path.rfolder,dont.read = "") {
	.Call('Rfast_check_usage', PACKAGE = 'Rfast',path.man,path.rfolder,dont.read)
}