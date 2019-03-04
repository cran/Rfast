
checkAliases <- function(path.man,path.rfolder) {
	.Call(Rfast_check_aliases,path.man,path.rfolder)
}