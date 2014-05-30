sourceBenchmarkLibs <- function(path="../libs/",local=TRUE,...)
	lapply(list.files(path,full.names=TRUE),source,local=local)