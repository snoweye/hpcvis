#' Plotting
#' 
#' Plot methods for \code{prof} class objects.
#' 
#' @param x 
#' \code{prof} class object
#' @param which 
#' Vector consisting of a subset of the integers 1, 2, 3, 4.
#' Determines which plots will be produced.
#' @param plot.type
#' for profiling mpiP including "timing", "stats1", "stats2",
#' "messages1", and "messages2".
#' @param title
#' The label for the plot title. Should be a character string of
#' your choice, \code{NULL} for no label, or left blank for
#' the default plot label.  In the latter case, this is chosen
#' based on the input data.
#' @param bar.label
#' logical; indicates whether or not numeric values of heights
#' of barplots should be included should .
#' 
#' @export
profplot <- function(x, which=1L:4L, plot.type="timing", title, bar.label=FALSE)
{
  if (missing(title))
    show.title <- TRUE
  else if (is.null(label))
    show.title <- FALSE
  else
    show.title <- TRUE
  
  if (x@profiler == 'fpmpi')
    ret <- plot_fpmpi(x, which=which, show.title=show.title, label=title)
  else if (x@profiler == 'mpip')
    ret <- plot_mpip(x, which=which, show.title=show.title,
              plot.type=plot.type, label=title, bar.label=bar.label)
  else if (x@profiler == 'tau')
    ret <- plot_tau(x, ..., which=which, show.title=show.title,
             plot.type=plot.type, label=label, bar.label=bar.label)
  else
    stop("Unknown profiler")
  
  grid::grid.newpage()
  grid::grid.draw(ret)
  
  invisible(ret)
}
