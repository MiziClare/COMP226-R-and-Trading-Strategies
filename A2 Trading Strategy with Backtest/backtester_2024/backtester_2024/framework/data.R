library(quantmod)

getFilename <- function(directory,index) {
    return(file.path(directory,
                     sprintf("%02d.csv",index)))
}

readFile <- function(filename) { 
    as.xts(read.zoo(filename,index.column=1,
                             header=TRUE,sep=','))
}

# @return List of xts objects in OHLCV format
getData  <- function(directory) {
    directory <- file.path("DATA",directory) 
    nseries <- length(list.files(directory))
    fnames <- sapply(1:nseries,  function(x) 
                        getFilename(directory,index=x))
    cat("Read",nseries, "series from", directory,"\n")
    return(lapply(fnames, readFile))
}
