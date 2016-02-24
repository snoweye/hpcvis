#' Plotting
#' 
#' Plot methods for \code{prof} class objects.
#' 
#' @param x 
#' \code{prof} class object
#' @param title
#' The label for the plot title. Should be a character string of
#' your choice, \code{NULL} for no label, or left blank for
#' the default plot label.  In the latter case, this is chosen
#' based on the input data.
#' @param which 
#' Vector consisting of a subset of the integers 1, 2, 3, 4.
#' Determines which plots will be produced.
#' @param plot.type
#' for profiling mpiP including "timings", "stats", and "messages".
#' @param stacked
#' Should bars be stacked or separated?  Only supported for the
#' \code{plot.type} values of "stats" or "messages".
#' @param bar.label
#' logical; indicates whether or not numeric values of heights
#' of barplots should be included should .
#' 
#' @export
profplot <- function(x, title, which=1L:4L, plot.type="timing", stacked=FALSE, bar.label=FALSE)
{
  if (class(x) != "prof")
    stop("argument 'x' must be of class 'prof'")
  
  plot.type <- match.arg(tolower(plot.type), c("timings", "stats", "messages"))
  if (plot.type == "stats")
    plot.type <- ifelse(stacked, "stats2", "stats1")
  if (plot.type == "messages")
    plot.type <- ifelse(stacked, "messages2", "messages1")
  
  if (missing(title))
    show.title <- TRUE
  else if (is.null(title))
    show.title <- FALSE
  else
    show.title <- TRUE
  
  if (x@profiler == 'fpmpi')
    ret <- plot_fpmpi(x, which=which, show.title=show.title, label=title)
  else if (x@profiler == 'mpip')
    ret <- plot_mpip(x, which=which, show.title=show.title, plot.type=plot.type, label=title, bar.label=bar.label, stacked=stacked)
  else if (x@profiler == 'tau')
    ret <- plot_tau(x, which=which, show.title=show.title, plot.type=plot.type, label=title, bar.label=bar.label)
  else
    stop("Unknown profiler")
  
  grid::grid.newpage()
  grid::grid.draw(ret)
  
  invisible(ret)
}
