##
## Function read.all.csv() reads all CSV files whose name match a pattern and 
## returns the data.frame that is a rbind() of all the CSV files read.
## The function will also keep the data.frame as a cache copy and will return
## this data.frame immediately if the cached copy exists.  Once a cache copy
## of the data.frame is created, it is saved away to disk.  If there is no
## cache copy of the data.frame in memory, then the data.frame will be restored
## from the disck cache of the data.frame. If neither cache copies exists, then
## the data.frame build by reading the individiual files matching the pattern 
## in the provided path.
## If file_cache_only is set to TRUE, then the memory cache is not used, and 
## the data.frame is cached on the disk (if cache is set/left to TRUE).

#' @export
read.all.csv<-function(path,pattern,skip=0,sep=",",comment.char="#",quote = "\"'",
      max.files=0,verbose=TRUE,filter=NULL,cache=TRUE,file_cache_only=FALSE,
      header=T, columns=NULL) {
   cache_name<-sprintf("cache_%s_%s",path,pattern);
   cache_file<-sprintf("%s.rds",cache_name);
   cache_file<-gsub("[^a-zA-Z0-9_\\.]","_",cache_file);

   # Check if there is a usable cache copy of the 
   # data.frame. The cache copy would either be in
   # memory or on disk.
   if (cache) {
      if (exists(cache_name)) {
         return(get(cache_name, envir = .GlobalEnv));
      } else {
         if (file.exists(cache_file)) {
            ret.data<-readRDS(file=cache_file);
            if (!file_cache_only) assign(cache_name, ret.data, envir = .GlobalEnv);
            return(ret.data);
         }
      }
   }

   ret.data<-data.frame();
   filelist<-list.files(path=path,pattern=pattern);
   if (verbose) {
      if (length(filelist) > 0) {
         writeLines(sprintf("The following files in %s match pattern \"%s\":",
                  path,pattern));
         for (file in filelist) {
            writeLines(sprintf("   File = %s",file));
         }
      }
   }

   if (!is.null(filter)) {
      if (class(filter) != "function") {
         writeLines("Argument filter expected to be a function");
         print(filter);
      }
   }

   if (max.files > 0 && length(filelist) > max.files) {
      if (verbose) {
         writeLines(
               sprintf(
         "Excess files will be skipped. Trimming list from %d to first %d:",
         length(filelist),max.files));
      }
      filelist<-filelist[1:max.files];
      if (verbose) {
         writeLines("Only the following files will be processed:");
         for (file in filelist) {
            writeLines(sprintf("   File = %s",file));
         }
      }
   }
   if (max.files < 0 && length(filelist) > -max.files) {
      if (verbose) {
         writeLines(
               sprintf(
         "Excess files will be skipped. Trimming list from %d to last %d:",
         length(filelist),-max.files));
      }
      filelist<-filelist[(length(filelist)+max.files):length(filelist)];
      if (verbose) {
         writeLines("Only the following files will be processed:");
         for (file in filelist) {
            writeLines(sprintf("   File = %s",file));
         }
      }
   }
   for (file in filelist) {
      new.data<-read.csv(sprintf("%s/%s",path,file),
            skip=skip,sep=sep,comment.char=comment.char,quote=quote,header=header);
      if (!is.null(columns)) {
         new.data<-new.data[,columns];
      }
      new.data$SOURCE_FILE<-file;
      if (!is.null(filter)) {
         new.data<-new.data;
      }

      ret.data<-rbind(ret.data,new.data);
      if (verbose) {
         writeLines(
               sprintf(
           "   File = %s with %d rows added to total making %d rows",
           file,nrow(new.data),nrow(ret.data)));
      }
   }
   if (cache) {
      if (!file_cache_only) assign(cache_name, ret.data, envir = .GlobalEnv);
      saveRDS(ret.data,file=cache_file);
   }
   return(ret.data);
}
