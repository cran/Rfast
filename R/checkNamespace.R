
checkNamespace <- function(path.namespace,path.rfolder) {
	.Call('Rfast_check_namespace', PACKAGE = 'Rfast',path.namespace,path.rfolder)
}