
checkTF <- function(path.man,dont.read = "") {
	.Call(Rfast_check_true_false,path.man,dont.read)
}