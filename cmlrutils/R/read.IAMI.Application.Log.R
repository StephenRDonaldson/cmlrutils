#
# Read IAMI Application Log - IAF Variant
#

#' @export
read.IAMI.Application.Log.IAF.Variant<-function(FILENAME) {
   cols<-c("PRI","VERSION","TIMESTAMP","HOSTNAME","APPLICATION","APP_PROCESS_ID","MESSAGE_ID","STRUCTURED_DATA","MESSAGE","PREVTIME");
###pipename<-sprintf("awk 'BEGIN {FS=\"~\";} $1 ~ /(INFO|WARN|ERROR)/ { printf(\"%%s\\n\",$0); }' %s",FILENAME);

   pipename<-sprintf("awk 'BEGIN {FS=\"~\"; OFS=\"\\t\";} $1 ~ /(DEBUG|INFO|WARN|ERROR)/ { $9=\"\"; if ($6 in applmap) { prev_time = applmap[$6]; } else { prev_time = $3; } applmap[$6] = $3; printf(\"%%s\\t%%s\\n\",$0,prev_time); }' %s",FILENAME);

   appldata<-read.table(pipe(pipename),sep="\t",col.names=cols);
   appldata$TIMESTAMP<-gsub(",",".",appldata$TIMESTAMP);
   appldata$PREVTIMEP<-gsub(",",".",appldata$PREVTIME);
   save_options<-options();
   options(digits.secs=3);
   appldata$TimeStamp<-as.POSIXct(appldata$TIMESTAMP,format="%d/%m/%Y %H:%M:%OS");
   appldata$PrevTime<-as.POSIXct(appldata$PREVTIME,format="%d/%m/%Y %H:%M:%OS");
   options(save_options);

   return(appldata);
}
#
# Read IAMI Application Log - EAI Variant
#

#' @export
read.IAMI.Application.Log.EAI.Variant<-function(FILENAME) {
   cols<-c("PRI","VERSION","TIMESTAMP","HOSTNAME","APPLICATION","APP_PROCESS_ID","MESSAGE_ID","STRUCTURED_DATA","GUID","MESSAGE","PREVTIME");
###pipename<-sprintf("awk 'BEGIN {FS=\"~\";} $1 ~ /(INFO|WARN|ERROR)/ { printf(\"%%s\\n\",$0); }' %s",FILENAME);

   pipename<-sprintf("awk 'BEGIN {FS=\"~\"; OFS=\"\\t\";} ($1 ~ /(DEBUG|INFO|WARN|ERROR)/) && ($9 ~ /[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}/) { $10=\"\"; if ($9 in applmap) { prev_time = applmap[$9]; } else { prev_time = $3; } applmap[$9] = $3; printf(\"%%s\\t%%s\\n\",$0,prev_time); }' %s",FILENAME);

   appldata<-read.table(pipe(pipename),sep="\t",col.names=cols);
   appldata$TIMESTAMP<-gsub(",",".",appldata$TIMESTAMP);
   appldata$PREVTIMEP<-gsub(",",".",appldata$PREVTIME);
   save_options<-options();
   options(digits.secs=3);
   appldata$TimeStamp<-as.POSIXct(appldata$TIMESTAMP,format="%d/%m/%Y %H:%M:%OS");
   appldata$PrevTime<-as.POSIXct(appldata$PREVTIME,format="%d/%m/%Y %H:%M:%OS");
   options(save_options);

   return(appldata);
}
