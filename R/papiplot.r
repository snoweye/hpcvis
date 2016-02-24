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
#' 
#' @return
#' A ggplot2 object.
#' 
#' @rdname papiplot
#' @export
papiplot <- function (x, ...)
{
  UseMethod("papiplot", x)
}
