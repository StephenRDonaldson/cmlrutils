library(ggplot2);
library(scales);

#' @export
TimeGraph<-function(GRAPH,BREAKS="1 hour",ANGLE=45) {
   if (BREAKS != "NA") {
      GRAPH<-GRAPH+scale_x_datetime(breaks=date_breaks(BREAKS));
   } else {
      GRAPH<-GRAPH+scale_x_datetime();
   }
   GRAPH<-GRAPH+theme(axis.text.x = element_text(angle = ANGLE, vjust = 0));
   GRAPH<-GRAPH+theme(legend.position="bottom");

   return(GRAPH);
}


#' @export
graph_cust<-function(plot,size=22) {
   plot<-plot+theme(plot.title=element_text(size=22));
   plot<-plot+theme(legend.text=element_text(size=18));
   plot<-plot+theme(strip.text=element_text(size=18));
   plot<-plot+theme(legend.title=element_text(size=22));
   plot<-plot+theme(axis.text=element_text(size=22));
   plot<-plot+theme(axis.title=element_text(size=22));
   plot<-plot+theme(legend.position="bottom");
   plot<-plot+expand_limits(y=0);
   return(plot);
}

#' @export
graph_cust_nolim<-function(plot,size=22) {
   plot<-plot+theme(plot.title=element_text(size=22));
   plot<-plot+theme(legend.text=element_text(size=18));
   plot<-plot+theme(strip.text=element_text(size=18));
   plot<-plot+theme(legend.title=element_text(size=22));
   plot<-plot+theme(axis.text=element_text(size=22));
   plot<-plot+theme(axis.title=element_text(size=22));
   plot<-plot+theme(legend.position="bottom");
   return(plot);
}

#' @export
ggplot_cust<-function() {
   return(graph_cust(ggplot()));
}
