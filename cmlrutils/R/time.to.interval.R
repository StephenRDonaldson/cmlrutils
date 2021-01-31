# TimeStamp - POSIXlt.
# IntervalInSeconds - number of seconds in an interval
# errormax - offset seconds to locate start of interval

#' @export
IntervalTime<-function(TimeStamp,IntervalInSeconds,errormax=0) {
   return(as.POSIXct(((as.numeric(TimeStamp)+errormax)%/%IntervalInSeconds)
             *IntervalInSeconds,origin="1970-01-01"));
}
