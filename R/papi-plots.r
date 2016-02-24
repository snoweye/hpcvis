#' Plots PAPI objects
#' 
#' @param x
#' PAPI object.
#' @param ...
#' Additional objects.
#' @param title
#' Optional argument for adding a title to the plot.
#' @param opnames
#' An optional argument to specify different names for the 
#' expressions/operations used to generate the profiler data.
#' @param color
#' Logical; indicates whether groups should be colored differently
#' in the case of plotting multiple objects
#' @param facet.by
#' Choice to facet cache plots by the different expressions/operations
#' (\code{facet.by="operation"}), or by the cache level (\code{facet.by="level"}).
#' 
#' @examples
#' \dontrun{
#' library(pbdPAPI)
#' x <- system.cache(rnorm(2e2))
#' y <- system.cache(rnorm(1e4))
#' z <- system.cache(rnorm(1e6))
#' 
#' library(hpcvis)
#' plot(x)
#' plot(x, opnames=NULL)
#' 
#' plot(x, y, opnames=c("2e2", "1e4"), title="rnorm()")
#' 
#' plot(x, y, z, opnames=c("rnorm(2e2)", "rnorm(1e4)", "rnorm(1e6)"))
#' 
#' plot(x, y, z, opnames=c("2e2", "1e4"))
#' }
#' 
#' @name papi_cache-plot
#' @rdname papi_cache-plot
#' @method plot papi_cache
#' @export
plot.papi_cache <- function(x, ..., title, opnames, color=FALSE, facet.by="operation")
{
  
  assert_that(is.flag(color))
  assert_that(is.string(facet.by))
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
    geom_text(data=df, aes(label=val, y=val), vjust=0) +
    theme_bw() +
    xlab(xlab) +
    ylab(paste("Cache", type))
  
  g <- g + facet_wrap(as.formula(paste("~", facetvar)))
  
  if (is.null(opnames))
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
