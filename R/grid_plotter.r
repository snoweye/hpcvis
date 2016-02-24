grid_plotter <- function(plots, which, label, show.title=TRUE, legend)
{
  add.legend <- !missing(legend)
  
  if (length(which) > 4 || length(which) < 1 || any(which < 0) || any(which > 4) || length(unique(which)) != length(which))
  {
    stop("argument 'which' must contain a subset of the numbers 1, 2, 3, 4")
  } 
  
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
