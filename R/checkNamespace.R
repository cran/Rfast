
checkNamespace <- function(path.namespace,path.rfolder) {
	.Call(Rfast_check_namespace,path.namespace,path.rfolder)
}