#' Plotting
#' 
#' Plot methods for \code{prof} class objects.
#' 
#' @param x 
#' \code{prof} class object
#' @param object 
#' \code{prof} class object
#' @param ...
#' extra arguments
#' @param which 
#' Vector consisting of a subset of the integers 1, 2, 3, 4.
#' Determines which plots will be produced.
#' @param show.title 
#' Logical; determines whether or not the plot title will be
#' shown.
#' @param plot.type
#' for profiling mpiP including "timing", "stats1", "stats2",
#' "messages1", and "messages2".
#' @param label
#' The label for the plot title. Should be a character string or
#' left missing for the default.
#' @param bar.label
#' logical; indicates whether or not numeric values of heights
#' of barplots should be included should .
#' 
#' @docType methods
#' @keywords Methods
#' @name prof-plot
#' @rdname prof-plot
NULL



#' @rdname prof-plot
#' @export
setMethod("plot", signature(x="prof"),
  function(x, ..., which=1L:4L, show.title=TRUE, plot.type="timing", label, bar.label=FALSE)
  {
    if (missing(label))
      show.title <- TRUE
    else if (is.null(label))
      show.title <- FALSE
    
    if (x@profiler == 'fpmpi')
      ret <- plot_fpmpi(x, ..., which = which, show.title = show.title,
                 plot.type = plot.type, label = label, bar.label = bar.label)
    else if (x@profiler == 'mpip')
    {
      ret <- plot_mpip(x, ..., which = which, show.title = show.title,
                plot.type = plot.type, label = label, bar.label = bar.label)
      
      grid::grid.newpage()
      grid::grid.draw(ret)
    }
    else if (x@profiler == 'tau')
      ret <- plot_tau(x, ..., which = which, show.title = show.title,
               plot.type = plot.type, label = label, bar.label = bar.label)
    else
      stop("Unknown profiler")
    
    return(ret)
  }
)



#' @rdname prof-plot
#' @export
autoplot.prof <- function(object, ...)
{ 
  plot(object, ...)
}
