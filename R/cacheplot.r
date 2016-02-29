#' Plots Cacahe Misses
#' 
#' @param opnames
#' An optional argument to specify different names for the 
#' expressions/operations used to generate the profiler data.
#' @param color
#' Logical; should different groups be colored?
#' @param bar.label
#' Logical; should numeric values of heights of bars be shown?
#' 
#' @examples
#' \dontrun{
#' library(pbdPAPI)
#' x <- system.cache(rnorm(1e4))
#' y <- system.cache(rnorm(4e4))
#' z <- system.cache(rnorm(8e4))
#' 
#' library(hpcvis)
#' papiplot(x)
#' papiplot(x, opnames=NULL)
#' 
#' opnames <- c("small", "medium", "large")
#' papiplot(x, y, z, opnames=opnames)
#' papiplot(x, y, z, opnames=opnames, color=TRUE, facet.by="level")
#' }
#' 
#' @rdname papiplot
#' @method papiplot papi_cache
#' @export
papiplot.papi_cache <- function(x, ..., title, opnames, color=FALSE, facet.by="operation", bar.label=FALSE, label.angle=0)
{
  assert_that(is.flag(color))
  assert_that(is.string(facet.by))
  assert_that(is.flag(bar.label))
  facet.by <- match.arg(tolower(facet.by), c("operation", "level"))
  
  tmp <- cache_lookup_type_levels(x=x)
  type <- tmp$type
  levels <- tmp$levels
  nlevels <- length(levels)
  
  l <- list(x, ...)
  check_classes(l)
  
  len <- length(l)
  val <- as.numeric(sapply(l, as.numeric))
  df <- data.frame(val=val, level=levels)
  
  shownames <- ifelse(!missing(opnames) && is.null(opnames), FALSE, TRUE)
  
  if (!missing(opnames) && !is.null(opnames))
  {
    if (length(opnames) != len)
      stop("Length of argument 'opnames' does not match number of inputs")
    else
      opnames <- as.vector(sapply(opnames, function(nm) rep(nm, nlevels)))
  }
  else
  {
    opnames <- sapply(l, function(y) attributes(y)$call)
    opnames <- as.vector(sapply(opnames, function(nm) rep(nm, nlevels)))
  }
  
  if (length(unique(opnames)) != len)
    stop("operation names are not unique.")
  
  opnames <- factor(opnames, levels=unique(opnames))
  
  df <- cbind(df, opnames=opnames)
  
  if (facet.by == "operation")
  {
    xvar <- "level"
    facetvar <- "opnames"
    xlab <- "Cache Level"
  }
  else if (facet.by == "level")
  {
    xvar <- "opnames"
    facetvar <- "level"
    xlab <- "Operation"
    
    df$level <- paste("Level", df$level)
  }
  
  yvar <- "val"
  
  if (len > 1)
  {
    if (color)
      g <- ggplot(data=df, aes_string(x=xvar, y=yvar, fill="opnames")) + 
           scale_fill_discrete(name="Operation")
    else
      g <- ggplot(data=df, aes_string(x=xvar, y=yvar))
  }
  else
    g <- ggplot(data=df, aes_string(x=xvar, y=yvar))
  
  g <- g + 
    geom_bar(stat="identity") +
    theme_bw() +
    theme(axis.text.x=element_text(angle=label.angle, hjust=1)) +
    xlab(xlab) +
    ylab(paste("Cache", type))
  
  if (bar.label)
    g <- g + geom_text(data=df, aes(label=val, y=val), vjust=0)
  
  g <- g + facet_wrap(as.formula(paste("~", facetvar)))
  
  if (!shownames)
  {
    if (facet.by == "operation")
      g <- g + theme(strip.background=element_blank(), strip.text.x=element_blank())
    else if (facet.by == "level")
      g <- g + theme(axis.ticks=element_blank(), axis.text.x=element_blank())
  }
  
  if (!missing(title))
  {
    if (!is.null(title))
      g <- g + ggtitle(title)
  }
  else
    g <- g + ggtitle("Total Cache Misses")
  
  return(g)
}



check_classes <- function(l)
{
  classes <- unique(as.vector(sapply(l, class)))
  if (length(classes) > 2)
    stop("You must pass in objects of the same type", call.=FALSE)
  
  return(TRUE)
}



cache_lookup_type_levels <- function(x)
{
  if (any(class(x) == "papi_cache"))
  {
    type <- sub(names(x)[1], pattern=".*cache ", replacement="")
    type <- gsub(type, pattern="(^|[[:space:]])([[:alpha:]])", replacement="\\1\\U\\2", perl=TRUE)
    levels <- gsub(names(x), pattern="(^L| cache misses)", replacement="")
  }
  
  ## ...
  return(list(type=type, levels=levels))
}
