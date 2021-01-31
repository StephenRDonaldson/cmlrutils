#' @export 
numberOfDays <- function(parm_date) {
   m <- format(parm_date, format="%m")

   while (format(parm_date, format="%m") == m) {
      parm_date <- parm_date + 1
   }

   return(as.integer(format(parm_date - 1, format="%d")))
}

#' @export 
isMonthEnd <- function(parm_date) {
   if (format(parm_date,"%a") == "Sun") return(FALSE);
   if ((format(parm_date,"%a") == "Sat")
         && (as.integer(format(parm_date,"%d"))+1 == numberOfDays(parm_date)))
      return(TRUE);
   if (as.integer(format(parm_date,"%d")) == numberOfDays(parm_date))
      return(TRUE);

   return(FALSE);
}

#' @export 
isWeekDay <- function(parm_date) {
   work_time<-as.POSIXct(parm_date);
   if (work_time$wday==0) return(FALSE);
   if (work_time$wday==6) return(FALSE);
   return(TRUE);
}

#' @export 
BatchDate <- function(time) {
   if (!inherits(time,"POSIXt")) {
      print("Invalid type for parameter time in function BatchDate");
      print("   Parameter time should inherit POSIXt, not ...");
      print(class(time));
      stop("Invalid type for parameter time in function BatchDate");
   }

   date <- as.Date(time-86400/2);
   return(date);
}

#' @export 
JulianToGregorianCYYDDDf <- function(JULIAN_DATE) {
   julian_date<-as.character(JULIAN_DATE);
   work_century<-as.numeric(substr(julian_date,1,1))*100+1900;
   work_year<-as.numeric(substr(julian_date,2,3))+work_century;
   work_day<-substr(julian_date,4,6);
   work_date<-as.Date(sprintf("%04d-%s",work_year,work_day),format="%Y-%j");
   return(work_date);
}

#' @export 
isPrimeTime<-function(TIMESTAMP) {
   work_time<-TIMESTAMP;
   if (!inherits(work_time,"POSIXlt")) {
      work_time<-as.POSIXct(work_time,origin="1970-01-01 00:00:00");
   }

   if (work_time$wday==0) return(FALSE);
   if (work_time$wday==6) return(FALSE);
   if (work_time$hour<08) return(FALSE);
   if (work_time$hour>16) return(FALSE);

   return(TRUE);
}

#' @export 
read_soa_events<-function(FILENAME) {
   evts<-read.csv(FILENAME);
   evts$T1<-as.POSIXct(evts$TIMESTAMP,format="%a %b %d %Y %H:%M:%S");
   ret_events<-data.frame(Description=evts$EVENT,LABEL=evts$LABEL,BEGIN=evts$T1,END=evts$T1);
   return(ret_events);
}

#' @export 
range_limit_on_timestamp<-function(DATAFRAME,STARTTIME,ENDTIME)  {
   starttime<-as.POSIXct(STARTTIME);
   endtime<-as.POSIXct(ENDTIME);
   return(DATAFRAME[DATAFRAME$TimeStamp>=starttime&DATAFRAME$TimeStamp<=endtime,]);
}

#' @export 
date_qualifier<-function() {
   if (!exists("cached_date_qualifier")) {
      assign("cached_date_qualifier", format(Sys.time(),format="D%Y%m%d_T%H%M%S"), envir = .GlobalEnv);

   }
   return(cached_date_qualifier);
}
