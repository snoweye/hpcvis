#' FLOPs Benchmark Plotter
#' 
#' @param groupby
#' Should operations be grouped by "color" or by "shape"?  The
#' latter is useful when black/white plots are needed.
#' 
#' @examples
#' \dontrun{
#' library(pbdPAPI)
#' data <- list(rnorm(1e4), rnorm(2e4), rnorm(3e4))
#' x <- flopsbench(exp, sqrt, sum, data=data)
#' 
#' library(hpcvis)
#' papiplot(x)
#' papiplot(x, groupby="shape")
#' }
#' 
#' @rdname papiplot
#' @method papiplot flopsbench
#' @export
papiplot.flopsbench <- function(x, ..., title, opnames, groupby="color")
{
  groupby <- match.arg(tolower(groupby), c("color", "shape"))
  
  if (!missing(opnames))
  {
    if (length(opnames) != length(x))
      stop("argument 'opnames' is of incorrect length")
    
    names(x) <- opnames
  }
  
  df <- lapply(x, as.data.frame)
  nm <- names(df)
  df <- lapply(1:length(df), function(i) cbind(df[[i]], nm[i]))
  df <- do.call(rbind, df)
  names(df)[length(df)] <- "Operation"
  df$operation <- factor(df$Operation)

  g <- ggplot(df, aes_string("n", "mflops", group="Operation")) + 
    theme_bw() + 
    ylab("Megaflops")
  
  if (groupby == "color")
    g <- g + geom_line(aes(color=Operation)) + geom_point(aes(color=Operation))
  else if (groupby == "shape")
    g <- g + geom_point(aes(shape=Operation))
  
  if (missing(title))
    g <- g + ggtitle("Floating Point Operations Benchmark")
  else if (!is.null(title))
    g <- g + ggtitle(title)
  
  g
}
