
AddToNamespace <- function(path.namespace,path.rfolder,sort = FALSE) {
	.Call('Rfast_add_to_namespace', PACKAGE = 'Rfast',path.namespace,path.rfolder,sort)
}