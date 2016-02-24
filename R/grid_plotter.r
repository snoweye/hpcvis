grid_plotter <- function(plots, which, label, show.title=TRUE, legend)
{
  add.legend <- !missing(legend)
  
  title <- grid::textGrob(label, gp=grid::gpar(fontsize=20, font=3))
  
  if (length(which) == 1) 
    ncol <- 1
  else
    ncol <- 2
  
  g <- do.call(arrangeGrob, c(plots[which], list(ncol=ncol)))
  
  if (!missing(legend))
    g <- arrangeGrob(g, legend, ncol=2, widths=c(10, 2))
  
  if (show.title)
    g <- arrangeGrob(title, g, heights=c(1, 10))
  
  return(g)
}
