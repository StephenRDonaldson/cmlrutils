#' @export
ibm.lspr.data<-function() {
   LSPR.URL<-"https://www-304.ibm.com/servers/resourcelink/lib03060.nsf/pages/srmindex";
   LSPR.TDATA<-readHTMLTable(LSPR.URL,header=T);
   return(LSPR.TDATA);
}

