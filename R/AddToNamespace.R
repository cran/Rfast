
AddToNamespace <- function(path.namespace,path.rfolder) {
	.Call('Rfast_add_functions_to_export', PACKAGE = 'Rfast',path.namespace,path.rfolder)
}