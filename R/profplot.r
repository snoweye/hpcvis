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
#' Vector consisting of a subset of the integers 1 through 4.
#' Determines which plots will be produced.
#' @param plot.type
#' Different default plot sets to generate (mpiP data only). 
#' Options are "timings", "stats", and "messages".
#' @param stacked
#' Logical; should bars be stacked or separated?
#' @param bar.label
#' Logical; should numeric values of heights of bars be shown?
#' 
#' @examples
#' \dontrun{
#' x <- pbdPROF::read.prof("mydata")
#' hpcvis::profplot(x)
#' }
#' 
#' @export
profplot <- function(x, title, which=1:4, plot.type="timing", stacked=FALSE, bar.label=FALSE)
{
  if (class(x) != "prof")
    stop("argument 'x' must be of class 'prof'")
  
  if (length(which) > 4 || length(which) < 1 || any(which < 0) || any(which > 4) || length(unique(which)) != length(which))
    stop("argument 'which' must contain a subset of the numbers 1, 2, 3, 4")
  
  assert_that(missing(title) || is.null(title) || is.string(title))
  assert_that(is.flag(stacked))
  assert_that(is.flag(bar.label))
  assert_that(is.string(plot.type))
  plot.type <- match.arg(tolower(plot.type), c("timings", "stats", "messages"))
  
  
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
