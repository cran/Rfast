
RemoveFromNamespace <- function(path.namespace,files.to.remove) {
	.Call('Rfast_remove_from_namespace', PACKAGE = 'Rfast',path.namespace,files.to.remove)
}