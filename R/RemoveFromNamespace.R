
RemoveFromNamespace <- function(path.namespace,files.to.remove) {
	.Call(Rfast_remove_from_namespace,path.namespace,files.to.remove)
}