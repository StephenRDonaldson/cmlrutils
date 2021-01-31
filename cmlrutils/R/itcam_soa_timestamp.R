
# Function ITCAMSOATimeStamp:  Conversion of ITCAM for SOA database time format to POSIXlt.

#' @export
ITCAMSOATimeStamp<-function(TIME,TZ="GMT",TZDIFF=0) {
   options(digits.secs=3);
   if (class(TIME) != "numeric") {
      print(sprintf("TIME parameter has invalid class. Should be numeric, not %s",class(TIME)));
      stop("Invalid type for parameter TIME in function ITCAMSOATimeStamp");
   }
   if (class(TZ) != "character") {
      print(sprintf("TZ parameter has invalid class. Should be string, not %s",class(TZ)));
      stop("Invalid type for parameter TZ in function ITCAMSOATimeStamp");
   }
   if (class(TZDIFF) != "numeric") {
      print(sprintf("TZDIFF parameter has invalid class. Should be numeric, not %s",class(TZDIFF)));
      stop("Invalid type for parameter TZDIFF in function ITCAMSOATimeStamp");
   }

   TextTimeStamp<-sprintf("%.3f",(TIME+19000000000000000)/1000);
   POSIXTimeStamp<-as.POSIXct(TextTimeStamp,format="%Y%m%d%H%M%S",tz=TZ)+TZDIFF;
   return(POSIXTimeStamp);
}
