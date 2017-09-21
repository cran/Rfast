
checkAliases <- function(path.man,path.rfolder,dont.read = "") {
	.Call('Rfast_check_aliases', PACKAGE = 'Rfast',path.man,path.rfolder,dont.read)
}