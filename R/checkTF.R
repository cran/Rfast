
checkTF <- function(path.man,dont.read = "") {
	.Call('Rfast_check_true_false', PACKAGE = 'Rfast',path.man,dont.read)
}