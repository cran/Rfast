
read.directory <- function(path.directory) {
	.Call('Rfast_read_directory', PACKAGE = 'Rfast',path.directory)
}