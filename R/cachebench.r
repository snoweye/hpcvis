#' Cache Miss Benchmark Plotter
#' 
#' @param levels
#' Which cache levels to display.
#' 
#' @examples
#' \dontrun{
#' library(pbdPAPI)
#' x <- cachebench(rnorm(1e4), rnorm(2e4), rnorm(3e4))
#' 
#' library(hpcvis)
#' papiplot(x)
#' papiplot(x, label.angle=15)
#' }
#' 
#' @rdname papiplot
#' @method papiplot cachebench
#' @export
papiplot.cachebench <- function(x, ..., title, levels=1:3, facet.by="operation", label.angle=0)
{
  tmp <- x
  tmp$summarystats <- NULL
  tmp$type <- NULL
  
  nm <- names(tmp)
  df <- do.call(rbind, lapply(1:length(tmp), function(i) data.frame(tmp[[i]], nm[i])))
  df <- df[, c(levels, 4)]
  
  colnames(df)[ncol(df)] <- "Test"
  colnames(df) <- gsub(colnames(df), pattern=".cache.misses", replacement="", fixed=TRUE)
  
  df <- cachemelt(df)
  
  if (facet.by == "level")
  {
    xvar <- "variable"
    facetvar <- "Test"
    xlab <- "Cache Level"
  }
  else if (facet.by == "operation")
  {
    xvar <- "Test"
    facetvar <- "variable"
    xlab <- "Operation"
    
    df$level <- paste("Level", df$level)
  }
  
  yvar <- "value"
  
  g <- 
    ggplot(df, aes_string(x=xvar, y=yvar)) + 
      stat_boxplot(geom ='errorbar')+
      geom_boxplot() + 
      theme(axis.text.x=element_text(angle=label.angle, hjust=1)) +
      xlab(xlab) + 
      ylab("") + 
      facet_wrap(as.formula(paste("~", facetvar)))
      # facet_wrap(~ variable)
  
  if (missing(title))
    g <- g + ggtitle(x$type)
  else if (!is.null(title))
    g <- g + ggtitle(title)
  
  g
}



cachemelt <- function(df)
{
  len <- ncol(df) - 1
  value <- sapply(sapply(1:len, function(i) df[, i]), c)
  nm <- names(df)
  variable <- as.character(sapply(sapply(1:len, function(i) rep(nm[i], nrow(df))), c))
  Test <- rep(df$Test, len)
  
  data.frame(Test=Test, variable=variable, value=value)
}
