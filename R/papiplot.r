#' PAPI Plotter
#' 
#' Create plots from PAPI performance counter data.
#' 
#' @details
#' One may wonder why we do not simply overload \code{plot()}. In fact
#' that was the original incarnation of this package.  However,
#' overloading \code{plot()} leads to several issues.  Simply,
#' the basic plot command is *too* overloaded.  This makes it
#' difficult to find documentation, view function arguments, etc.
#' Using \code{papiplot()}, finding help is easier, and one can
#' also enjoy function argument autocompletion via the tab key.
#' 
#' @param x
#' PAPI object.
#' @param ...
#' Additional objects.
#' @param title
#' The label for the plot title. Should be a character string of
#' your choice, \code{NULL} for no label, or left blank for
#' the default plot label.  In the latter case, this is chosen
#' based on the input data.
#' @param facet.by
#' Choice to facet cache plots by the different expressions/operations
#' (\code{facet.by="operation"}), or by the cache level (\code{facet.by="level"}).
#' @param label.angle
#' The angle of x-axis labels.
#' 
#' @return
#' A ggplot2 object.
#' 
#' @rdname papiplot
#' @export
papiplot <- function (x, ..., title, facet.by="operation", label.angle=0)
{
  if (!missing(title))
    assert_that(is.null(title) || is.string(title))
  
  assert_that(is.numeric(label.angle))
  assert_that(is.string(facet.by))
  facet.by <- match.arg(tolower(facet.by), c("operation", "level"))
  
  UseMethod("papiplot", x)
}
