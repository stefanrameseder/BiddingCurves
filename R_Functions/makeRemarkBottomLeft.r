
makeRemarkBottomLeft <- function(footnoteText=format(Sys.time(), "%d %b %Y"),size= .7, color="black" ){
  require(grid)
  pushViewport(viewport())
  grid.text(label= footnoteText ,
            x = unit(1,"npc") - unit(25, "cm"),
            y= unit(2, "mm"),
            just=c("left", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}