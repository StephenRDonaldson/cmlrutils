# Function arg_min returns the argument that achieves the minimum of the 
# supplied function. The argument that appears first in the list which 
# acieves this minimum is returned.
#
#' @export
arg_min<-function(arguments,fun,...) {
   fun_evals<-data.frame(Arguments=arguments,Results=unlist(lapply(arguments,fun,...)));
   fun_evals$Index<-1:nrow(fun_evals);
   fun_evals<-fun_evals[order(fun_evals$Results,fun_evals$Index),];
   return(fun_evals[1,]$Arguments);
}
   
