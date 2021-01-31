#' @export
SDFromTriple<-function(n,sum,sumssquared) {
   return(sqrt(sumssquared/n-(sum/n)**2))
}
SDFromTripleDiff<-function(first_n,first_sum,first_sumssquared,
      last_n,last_sum,last_sumssquared) {
   return(SDFromTriple(last_n-first_n,last_sum-first_sum,
            last_sumssquared-first_sumssquared));
}
