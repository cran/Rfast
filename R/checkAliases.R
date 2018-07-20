
checkAliases <- function(path.man,path.rfolder,dont.read = "") {
	.Call(Rfast_check_aliases,path.man,path.rfolder,dont.read)
}