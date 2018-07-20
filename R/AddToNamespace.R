
AddToNamespace <- function(path.namespace,path.rfolder,sort = FALSE,dontread = "") {
	.Call(Rfast_add_to_namespace,path.namespace,path.rfolder,sort,dontread)
}