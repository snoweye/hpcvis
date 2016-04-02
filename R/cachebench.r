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
papiplot.cachebench <- function(x, ..., title, opnames, facet.by="operation", label.angle=0, levels=1:3)
{
  l <- list(...)
  if (length(l) != 0)
    stop("'...' arguments not supported in papiplot() for cachebench objects.")
  
  if (!missing(opnames) && !is.null(opnames))
    {
      nm <- names(x)
      opnames_len <- length(nm) - 2L
      
      if (length(opnames) != opnames_len)
        stop("length of argument 'opnames' doesn't match the number of operations in 'x'")
      
      names(x)[1:opnames_len] <- opnames
    }
  
  tmp <- x
  tmp$summarystats <- NULL
  tmp$type <- NULL
  
  nm <- names(tmp)
  df <- do.call(rbind, lapply(1:length(tmp), function(i) data.frame(tmp[[i]], nm[i])))
  
  names.df <- names(df)
  levels.measured <- unique(substr(names.df[grepl(names.df, pattern="^L")], 2, 2))
  levels.measured <- sapply(levels.measured, function(i) grep(names.df, pattern=paste0("L", i)))
  levels <- intersect(levels, levels.measured)
  if (length(levels) == 0)
    stop("no selected levels in argument 'levels' match recorded levels in data 'x'")
  df <- df[, c(levels, ncol(df))]
  
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
      theme_bw() + 
      stat_boxplot(geom ='errorbar')+
      geom_boxplot() + 
      theme(axis.text.x=element_text(angle=label.angle, hjust=1)) +
      xlab(xlab) + 
      ylab("Cache Misses") + 
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
