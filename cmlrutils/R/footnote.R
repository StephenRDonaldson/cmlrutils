#' @export
makeFootnote <- function(footnoteText= "_DEFAULT",size= .7, color= grey(.5))
   {
   if (footnoteText=="_DEFAULT") {
      footnoteText =
         sprintf("www.codemagus.com - %s",format(Sys.time(), "%d %b %Y"))
   }
   require(grid)
   pushViewport(viewport())
   grid.text(label= footnoteText ,
   x = unit(1,"npc") - unit(2, "mm"),
   y= unit(2, "mm"),
   just=c("right", "bottom"),
   gp=gpar(cex= size, col=color))
   popViewport()
   }

#' @export
CMLCopyRight <- function(footnoteText= "_DEFAULT",size= .7, color= grey(.5))
   {
   if (footnoteText=="_DEFAULT") {
      footnoteText =
         sprintf("Copyright (c) %s Code Magus Limited. All rights reserved.",format(Sys.time(),"%Y"))
   }
   require(grid)
   pushViewport(viewport())
   grid.text(label= footnoteText ,
   x = unit(1,"npc") - unit(2, "mm"),
   y= unit(2, "mm"),
   just=c("right", "bottom"),
   gp=gpar(cex= size, col=color))
   popViewport()
   }

#' @export
SDLCopyRight <- function(footnoteText= "_DEFAULT",size= .7, color= grey(.5))
   {
   if (footnoteText=="_DEFAULT") {
      footnoteText =
         sprintf("Copyright (c) %s Stephen Donaldson Limited. All rights reserved.",format(Sys.time(),"%Y"))
   }
   require(grid)
   pushViewport(viewport())
   grid.text(label= footnoteText ,
   x = unit(1,"npc") - unit(2, "mm"),
   y= unit(2, "mm"),
   just=c("right", "bottom"),
   gp=gpar(cex= size, col=color))
   popViewport()
   }

#' @export
SRDCopyRight <- function(footnoteText= "_DEFAULT",size= .7, color= grey(.5))
   {
   if (footnoteText=="_DEFAULT") {
      footnoteText =
         sprintf("Copyright (c) %s Stephen R. Donaldson. All rights reserved.",format(Sys.time(),"%Y"))
   }
   require(grid)
   pushViewport(viewport())
   grid.text(label= footnoteText ,
   x = unit(1,"npc") - unit(2, "mm"),
   y= unit(2, "mm"),
   just=c("right", "bottom"),
   gp=gpar(cex= size, col=color))
   popViewport()
   }
