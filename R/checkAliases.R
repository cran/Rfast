
checkAliases <- function(path.man,path.rfolder) {
	.Call('Rfast_check_aliases', PACKAGE = 'Rfast',path.man,path.rfolder)
}