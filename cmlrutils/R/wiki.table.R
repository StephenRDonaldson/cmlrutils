# Original package: hacks

#' @export
wiki.table<-function(x,caption=deparse(substitute(x)),...)
{
    if(!is.matrix(x) & !is.data.frame(x))
        stop("x must be a matrix or a data frame")
    parms<-list(...)
    if(length(parms)){
        parms.vals <- paste("\"", unlist(parms), "\"", sep = "")
        parms <- paste(names(parms), parms.vals, sep = "=", collapse = " ")
    }
    cat("{|  class=\"wikitable\" style=\"font-family: Courier New;\"", parms, "\n")
    if(!is.null(caption)) 
        cat("|+", caption, "\n")
    if(length(colnames(x))){
        col.head <- paste(colnames(x), collapse = " !! ")
        if(length(rownames(x))) col.head <- paste("!!", col.head)
        cat("!", col.head, "\n")
        cat("|-\n")
    }
    for(i in 1:nrow(x)){
        if(length(rownames(x))) cat("!", rownames(x)[i], "\n")
        cell.dat <- paste(x[i, ], collapse = " || ")
        cat("|", cell.dat, "\n")
        if(i < nrow(x)) cat("|-\n")
    }
    cat("|}\n")
}
