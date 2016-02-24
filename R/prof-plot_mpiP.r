### Timings by Rank
plot_mpip_timing <- function(output, bar.label, plot.type)
{
  ### Fool R CMD check
  Rank <- MPI_time <- Tot <- Call1 <- Time <- Call2 <-
  Time_per <- Count <- Call_Name <- Mean_time <- Max_time <-
  Mpi_per <- Min_time <- Min <- Mean <- Max <- Sum <- NULL
  rm(list = c("Rank", "MPI_time", "Tot", "Call1", "Time", "Call2",
              "Time_per", "Count", "Call_Name", "Mean_time", "Max_time",
              "Mpi_per", "Min_time", "Min", "Mean", "Max", "Sum"))
  
  
  rankvsmpi <- output[[1]]
  rankvsmpi <- rankvsmpi[(rankvsmpi$Task != "*"), ]
  commsize <- nrow(rankvsmpi)
  
  rankvsmpi1 <- data.frame(Rank=rankvsmpi$Task, MPI_time=rankvsmpi$MPITime)
  
  apptime <- rankvsmpi$AppTime - rankvsmpi$MPITime
  ranker <- data.frame(Rank = rankvsmpi$Task, MPI_time = apptime)
  rankvsmpi1 <- rbind(ranker, rankvsmpi1)
  Timing <- factor(c(rep("Other", length(unique(rankvsmpi1$Rank))), rep("MPI", length(unique(rankvsmpi1$Rank)))))
  rankvsmpi1 <- cbind(rankvsmpi1, Timing)
  rankvsmpi1 <- rankvsmpi1[order(rankvsmpi1$Timing), ]
  
  runtime <- rankvsmpi1
  runtime$MPI_time[which(runtime$Timing=="Other")] <- rankvsmpi$AppTime
  
  g1 <- ggplot(data=rankvsmpi1, aes(Rank, MPI_time, fill=Timing)) +
          geom_bar(stat="identity") +
          ylab("Application Run Time (seconds)") + 
          theme_bw() + 
          theme(legend.direction="horizontal", 
            plot.margin=unit(c(1, 0, 0, 0), "cm"), 
            legend.position=c(0.5, 1.08))
  
  # Percentage time by rank
  tot <- rankvsmpi$AppTime
  rankvsmpi2 <- rankvsmpi1
  rankvsmpi2$MPI_time <- rankvsmpi2$MPI_time / tot[as.numeric(as.character(rankvsmpi2$Rank))+1] * 100
  
  pctruntime <- rankvsmpi2
  pctruntime$MPI_time <- round(pctruntime$MPI_time, 2)
  pctruntime$Tot <- pctruntime$MPI_time
  pctruntime$Tot[which(pctruntime$Timing=="Other")] <- rep(100, commsize)
  
  g2 <- ggplot(data=rankvsmpi2, aes(Rank, MPI_time, fill=Timing)) +
          geom_bar(stat="identity") +
          ylab("% Application Run Time") + 
          theme_bw() + 
          theme(legend.direction="horizontal", 
            plot.margin=unit(c(1, 0, 0, 0), "cm"), 
            legend.position=c(0.5, 1.08))
  
  # Run time by function
  timestat <- output[[3]]
  timevscallname <- data.frame(Call1 = timestat$Call, Time = timestat$Time*1000)
  timevscallname <- aggregate(timevscallname[, "Time"], timevscallname["Call1"], FUN=sum)
  names(timevscallname) <- c("Call1", "Time")
  Legends1 <- factor(timevscallname$Call1)
  
  g3 <- ggplot(data=timevscallname, aes(Call1, Time, fill=Legends1)) + 
          geom_bar(stat="identity") +
          xlab("MPI Function") + 
          ylab("MPI Function Run Time (seconds)") +
          theme_bw() + 
          theme(legend.position="none") +
          theme(axis.text.x=element_text(angle=-30, vjust=0.5))
  
  # Percentage of run time by function
  timevscallname_per <- data.frame(Call2=timestat$Call, Time_per=timestat$MPI.)
  timevscallname_per <- aggregate(timevscallname_per[, "Time_per"], timevscallname_per["Call2"], FUN=sum)
  names(timevscallname_per) <- c("Call2", "Time_per")
  Legends2 <- factor(timevscallname_per$Call2)
  
  g4 <- ggplot(data=timevscallname_per, aes(Call2, Time_per, fill=Legends2)) + 
          geom_bar(stat="identity") +
          xlab("MPI Function") + 
          ylab("% MPI Function Run Time") +
          theme_bw() + 
          theme(legend.position="none") +
          theme(axis.text.x=element_text(angle=-30, vjust=0.5))
  
  
  if (bar.label)
  {
    g1 <- g1 + geom_text(data=runtime, aes(label=MPI_time, y=MPI_time), size=3, vjust=0)
    g2 <- g2 + geom_text(data=pctruntime, aes(label=MPI_time, y=Tot), size=3, vjust=0)
    g3 <- g3 + geom_text(data=timevscallname, aes(label=Time, y=Time), size=3, vjust=0)
    g4 <- g4 + geom_text(data=timevscallname_per, aes(label=Time_per, y=Time_per), size=3, vjust=0)
  }
  
  return( list(g1=g1, g2=g2, g3=g3, g4=g4) )
}



### Timing statistics
plot_mpip_stats <- function(output, bar.label, plot.type)
{
  ### Fool R CMD check
  Rank <- MPI_time <- Tot <- Call1 <- Time <- Call2 <-
  Time_per <- Count <- Call_Name <- Mean_time <- Max_time <-
  Mpi_per <- Min_time <- Min <- Mean <- Max <- Sum <- NULL
  rm(list = c("Rank", "MPI_time", "Tot", "Call1", "Time", "Call2",
              "Time_per", "Count", "Call_Name", "Mean_time", "Max_time",
              "Mpi_per", "Min_time", "Min", "Mean", "Max", "Sum"))
  
  
  timing <- output[[5]]
  timing <- timing[(timing$Rank != "*"), ]
  rownames(timing) <- 1:nrow(timing)
  timing$Rank <- factor(as.numeric(as.character(timing$Rank)))
  
  # Convert to seconds
  timing$Max <- signif(timing$Max/1000, digits=3)
  timing$Mean <- signif(timing$Mean/1000, digits=3)
  timing$Min <- signif(timing$Min/1000, digits=3)
  
  timingcount <- data.frame(Call_Name = timing$Name, Rank = timing$Rank, Count = timing$Count)
  timingcount <- aggregate(timingcount[, "Count"], timingcount[c("Rank", "Call_Name")], FUN=sum)
  names(timingcount) <- c("Rank", "Call_Name", "Count")
  
  timingmin <- data.frame(Call_Name = timing$Name, Rank = timing$Rank, Min_time = timing$Min)
  timingmin <- aggregate(timingmin[, "Min_time"], timingmin[c("Rank", "Call_Name")], FUN=min)
  names(timingmin) <- c("Rank", "Call_Name", "Min_time")
  
  timingmean <- data.frame(Call_Name = timing$Name, Rank = timing$Rank, Mean_time = timing$Mean)
  # the mean of means; I know this might not be the mean, but w/e
  timingmean <- aggregate(timingmean[, "Mean_time"], timingmean[c("Rank", "Call_Name")], FUN=mean)
  names(timingmean) <- c("Rank", "Call_Name", "Mean_time")
  timingmean$Mean_time <- signif(timingmean$Mean_time, digits=3)
  
  timingmax <- data.frame(Call_Name = timing$Name, Rank = timing$Rank, Max_time = timing$Max)
  timingmax <- aggregate(timingmax[, "Max_time"], timingmax[c("Rank", "Call_Name")], FUN=max)
  names(timingmax) <- c("Rank", "Call_Name", "Max_time")
  
  if (plot.type == "stats1")
  {
    # Function call count by rank
    g1 <- ggplot(data=timingcount, aes(Rank, Count, fill=factor(Call_Name))) + 
            geom_bar(stat="identity") + 
            ylab("Number of Function Calls") +
            theme_bw() + 
            theme(legend.position = "none") +
            facet_wrap(facets =~ Call_Name, scales = "free_x")
    
    # Min run time by rank
    g2 <- ggplot(data=timingmin, aes(Rank, Min_time, fill=factor(Call_Name))) + 
            geom_bar(stat="identity") + 
            ylab("Min Time (seconds)") +
            theme_bw() + 
            theme(legend.position = "none") +
            facet_wrap(facets =~ Call_Name, scales = "free_x")
     
    # Mean run time by rank
    g3 <- ggplot(data=timingmean, aes(Rank, Mean_time, fill=factor(Call_Name))) + 
            geom_bar(stat="identity") + 
            ylab("Mean Time (seconds)") +
            theme_bw() + 
            theme(legend.position = "none") +
            facet_wrap(facets =~ Call_Name, scales = "free_x")
    
    # Max run time by rank
    g4 <- ggplot(data=timingmax, aes(Rank, Max_time, fill=factor(Call_Name))) + 
            geom_bar(stat="identity") + 
            ylab("Max Time (seconds)") +
            theme_bw() + 
            theme(legend.position = "none") +
            facet_wrap(facets =~ Call_Name, scales = "free_x")
    
    if (bar.label)
    {
      g1 <- g1 + geom_text(data=timingcount, aes(label=Count, y=Count), size = 3, vjust=0)
      g2 <- g2 + geom_text(data=timingmin, aes(label=Min_time, y=Min_time), size = 3, vjust=0)
      g3 <- g3 + geom_text(data=timingmean, aes(label=Mean_time, y=Mean_time), size=3, vjust=0)
      g4 <- g4 + geom_text(data=timingmax, aes(label=Max_time, y=Max_time), size=3, vjust=0)
    }
    
    return( list(g1=g1, g2=g2, g3=g3, g4=g4) )
  }
  
  else if (plot.type == "stats2")
  {
    # Sum of MPI function count by rank
    g1 <- ggplot(data=timingcount, aes(Rank, Count, fill=factor(Call_Name))) + 
            geom_bar(stat="identity") + 
            theme_bw() + 
            ylab("Number of Function Calls")
    
    # Sum of Min run time by rank
    g2 <- ggplot(data=timingmin, aes(Rank, Min_time, fill=factor(Call_Name))) +
            geom_bar(stat="identity") + 
            ylab("Sum of Min Run Times (seconds)") +
            theme_bw() + 
            theme(legend.position = "none") + 
            scale_fill_discrete(name="MPI Function")
    
    # Sum of Mean run time by rank
    g3 <- ggplot(data=timingmean, aes(Rank, Mean_time, fill=factor(Call_Name))) + 
                geom_bar(stat="identity") + 
                ylab("Sum of Mean Run Time (seconds)") +
                theme_bw() + 
                theme(legend.position = "none") + 
                scale_fill_discrete(name="MPI Function")
    
    # Sum of Max run time by rank
    g4 <- ggplot(data=timingmax, aes(Rank, Max_time, fill=factor(Call_Name))) +
                geom_bar(stat="identity") + 
                ylab("Sum of Max Run Time (seconds)") +
                theme_bw() + 
                theme(legend.position = "none") + 
                scale_fill_discrete(name="MPI Function")
    
    if (bar.label)
    {
      commsize <- length(unique(timingcount$Rank))
      
      timingcount_label <- aggregate(timingcount[, "Count"], timingcount["Rank"], FUN=sum)
      names(timingcount_label) <- c("Rank", "Count")
      timingcount_label$Call_Name <- rep(as.character(timingcount$Call_Name[1]), commsize)
      g1 <- g1 + geom_text(data=timingcount_label, aes(label=Count, y=Count), size=3, vjust=0)
      
      timingmin_label <- aggregate(timingmin[, "Min_time"], timingmin["Rank"], FUN=sum)
      names(timingmin_label) <- c("Rank", "Min_time")
      timingmin_label$Call_Name <- rep(as.character(timingmin$Call_Name[1]), commsize)
      g2 <- g2 + geom_text(data=timingmin_label, aes(label=Min_time, y=Min_time), size=3, vjust=0)
      
      timingmean_label <- aggregate(timingmean[, "Mean_time"], timingmean["Rank"], FUN=sum)
      names(timingmean_label) <- c("Rank", "Mean_time")
      timingmean_label$Call_Name <- rep(as.character(timingmean$Call_Name[1]), commsize)
      g3 <- g3 + geom_text(data=timingmean_label, aes(label=Mean_time, y=Mean_time), size=3, vjust=0)
      
      timingmax_label <- aggregate(timingmax[, "Max_time"], timingmax["Rank"], FUN=sum)
      names(timingmax_label) <- c("Rank", "Max_time")
      timingmax_label$Call_Name <- rep(as.character(timingmax$Call_Name[1]), commsize)
      g4 <- g4 + geom_text(data=timingmax_label, aes(label=Max_time, y=Max_time), size=3, vjust=0)
    }
    
    
    # Plot a single legend
    g1 <- g1 + labs(fill = "MPI Call")
    tmp <- ggplot_gtable(ggplot_build(g1))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    
    g1 <- g1 + theme(legend.position = "none")
    
    return( list(g1=g1, g2=g2, g3=g3, g4=g4, legend=legend) )
  }
}



### Message statistics
plot_mpip_messages <- function(output, bar.label, plot.type)
{
  ### Fool R CMD check
  Rank <- MPI_time <- Tot <- Call1 <- Time <- Call2 <-
  Time_per <- Count <- Call_Name <- Mean_time <- Max_time <-
  Mpi_per <- Min_time <- Min <- Mean <- Max <- Sum <- NULL
  rm(list = c("Rank", "MPI_time", "Tot", "Call1", "Time", "Call2",
              "Time_per", "Count", "Call_Name", "Mean_time", "Max_time",
              "Mpi_per", "Min_time", "Min", "Mean", "Max", "Sum"))
  
  
  message <- output[[6]]
    
  messagemin <- data.frame(Call_Name = message$Name, Rank = message$Rank, Min = message$Min)
  messagemin <- messagemin[(messagemin$Rank !=  "*"),]
  
  messagemean <- data.frame(Call_Name = message$Name, Rank = message$Rank, Mean = message$Mean)
  messagemean <- messagemean[(messagemean$Rank !=  "*"),]
  
  messagemax <- data.frame(Call_Name = message$Name, Rank = message$Rank, Max = message$Max)
  messagemax <- messagemax[(messagemax$Rank !=  "*"),]
  
  messagesum <- data.frame(Call_Name = message$Name, Rank = message$Rank, Sum = message$Sum)
  messagesum <- messagesum[(messagesum$Rank !=  "*"),]
  
  
  if (plot.type == "messages1")
  {
    # Min message size by rank
    g1 <- ggplot(data = messagemin, aes(Rank, Min, fill = factor(Call_Name))) + 
            geom_bar(stat="identity") + 
            ylab("Min Message Size (bytes)") +
            theme_bw() + 
            theme(legend.position = "none") +
            facet_wrap(facets =~ Call_Name, scales = "free_x")
    
    # Mean message size by rank
    g2 <- ggplot(data = messagemean, aes(Rank, Mean, fill = factor(Call_Name))) + 
            geom_bar(stat="identity") + 
            ylab("Mean Message Size (bytes)") +
            theme_bw() + 
            theme(legend.position = "none") +
            facet_wrap(facets =~ Call_Name, scales = "free_x")
    
    # Max message size by rank
    g3 <- ggplot(data = messagemax, aes(Rank, Max, fill = factor(Call_Name))) + 
            geom_bar(stat="identity") + 
            ylab("Max Message Size (bytes)") +
            theme_bw() + 
            theme(legend.position = "none") +
            facet_wrap(facets =~ Call_Name, scales = "free_x")
    
    # Total message size by rank
    g4 <- ggplot(data = messagesum, aes(Rank, Sum, fill = factor(Call_Name))) + 
            geom_bar(stat="identity") + 
            ylab("Total Message Size (bytes)") +
            theme_bw() + 
            theme(legend.position = "none") +
            facet_wrap(facets =~ Call_Name, scales = "free_x")
    
    if (bar.label)
    {
      messagemin <- aggregate(messagemin$Min, by=list(messagemin$Rank, messagemin$Call_Name), FUN=function(x) sum(as.numeric(x)))
      colnames(messagemin) <- c("Rank", "Call_Name", "Min")
      messagemean <- aggregate(messagemean$Mean, by=list(messagemean$Rank, messagemean$Call_Name), FUN=function(x) sum(as.numeric(x)))
      colnames(messagemean) <- c("Rank", "Call_Name", "Mean")
      messagemax <- aggregate(messagemax$Max, by=list(messagemax$Rank, messagemax$Call_Name), FUN=function(x) sum(as.numeric(x)))
      colnames(messagemax) <- c("Rank", "Call_Name", "Max")
      messagesum <- aggregate(messagesum$Sum, by=list(messagesum$Rank, messagesum$Call_Name), FUN=function(x) sum(as.numeric(x)))
      colnames(messagesum) <- c("Rank", "Call_Name", "Sum")
      
      g1 <- g1 + geom_text(data = messagemin, aes(label = Min, y = Min), size = 3, vjust=0)
      g2 <- g2 + geom_text(data = messagemean, aes(label = Mean, y = Mean), size = 3, vjust=0)
      g3 <- g3 + geom_text(data = messagemax, aes(label = Max, y = Max), size = 3, vjust=0)
      g4 <- g4 + geom_text(data = messagesum, aes(label = Sum, y = Sum), size = 3, vjust=0)
    }
    
    return( list(g1=g1, g2=g2, g3=g3, g4=g4) )
  }
  
  else if (plot.type == "messages2")
  {
    # Total message size by rank
    g1 <- ggplot(data = messagesum, aes(Rank, Sum, fill = factor(Call_Name))) + 
            geom_bar(stat="identity") + 
            theme_bw() + 
            ylab("Total Message Size (bytes)") + 
            scale_fill_discrete(name="MPI Function")
    
    # Min message size by rank
    g2 <- ggplot(data = messagemin, aes(Rank, Min, fill = factor(Call_Name))) +
            geom_bar(stat="identity") + 
            ylab("Min Message Size (bytes)") +
            scale_fill_discrete(name="MPI Function") + 
            theme_bw() + 
            theme(legend.position = "none")
    
    # Mean message size by rank
    g3 <- ggplot(data = messagemean, aes(Rank, Mean, fill = factor(Call_Name))) +
            geom_bar(stat="identity") + 
            ylab("Mean Message Size (bytes)") + 
            scale_fill_discrete(name="MPI Function") + 
            theme_bw() + 
            theme(legend.position = "none")
    
    # Max message size by rank
    g4 <- ggplot(data = messagemax, aes(Rank, Max, fill = factor(Call_Name))) +
            geom_bar(stat="identity") + 
            ylab("Max Message Size (bytes)") + 
            scale_fill_discrete(name="MPI Function") + 
            theme_bw() + 
            theme(legend.position = "none")
    
    if (bar.label)
    {
      commsize <- length(unique(messagemin$Rank))
      
      messagesum_label <- aggregate(messagesum[, "Sum"], messagesum["Rank"], FUN=sum)
      names(messagesum_label) <- c("Rank", "Sum")
      messagesum_label$Call_Name <- rep(as.character(messagesum$Call_Name[1]), commsize)
      g1 <- g1 + geom_text(data=messagesum_label, aes(label = Sum, y = Sum), size = 3, vjust=0)
      
      messagemin_label <- aggregate(messagemin[, "Min"], messagemin["Rank"], FUN=sum)
      names(messagemin_label) <- c("Rank", "Min")
      messagemin_label$Call_Name <- rep(as.character(messagemin$Call_Name[1]), commsize)
      g2 <- g2 + geom_text(data=messagemin_label, aes(label = Min, y = Min), size = 3, vjust=0)
      
      messagemean_label <- aggregate(messagemean[, "Mean"], messagemean["Rank"], FUN=sum)
      names(messagemean_label) <- c("Rank", "Mean")
      messagemean_label$Call_Name <- rep(as.character(messagemean$Call_Name[1]), commsize)
      g3 <- g3 + geom_text(data = messagemean_label, aes(label = Mean, y = Mean), size = 3, vjust=0)
      
      messagemax_label <- aggregate(messagemax[, "Max"], messagemax["Rank"], FUN=sum)
      names(messagemax_label) <- c("Rank", "Max")
      messagemax_label$Call_Name <- rep(as.character(messagemax$Call_Name[1]), commsize)
      g4 <- g4 + geom_text(data = messagemax_label, aes(label = Max, y = Max), size = 3, vjust=0)
    }
    
    # Plot a single legend
    g1 <- g1 + labs(fill = "MPI Call")
    tmp <- ggplot_gtable(ggplot_build(g1))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    
    g1 <- g1 + theme(legend.position = "none")
    
    return( list(g1=g1, g2=g2, g3=g3, g4=g4, legend=legend) )
  }
}



### Counts FIXME
plot_mpip_counts <- function(output, bar.label, plot.type)
{
  ### Fool R CMD check
  Rank <- MPI_time <- Tot <- Call1 <- Time <- Call2 <- Call3 <-
  Time_per <- Count <- Call_Name <- Mean_time <- Max_time <-
  Mpi_per <- Min_time <- Min <- Mean <- Max <- Sum <- 
  Message_size <- Message_size_per <- NULL
  rm(list = c("Rank", "MPI_time", "Tot", "Call1", "Time", "Call2", "Call3",
              "Time_per", "Count", "Call_Name", "Mean_time", "Max_time",
              "Mpi_per", "Min_time", "Min", "Mean", "Max", "Sum", 
              "Message_size", "Message_size_per"))
  
  
  timing <- output[[5]]
  timingcount <- data.frame(Call_Name = timing$Name, Rank = timing$Rank, Count = timing$Count)
  timingcount <- timingcount[(timingcount$Rank != "*"),]
  
  message <- output[[6]]
  messagecount <- data.frame(Call_Name=message$Name, Rank=message$Rank, Count=message$Count)
  messagecount <- messagecount[(messagecount$Rank != "*"),]
  
  # (function call) count by rank
  g1 <- ggplot(data=timingcount, aes(Rank, Count, fill=factor(Call_Name))) + 
          geom_bar(stat="identity") + 
          theme_bw() + 
          ylab("Number of Function Calls") +
          scale_fill_discrete(name="MPI Function") 
  
  g2 <- ggplot(data = messagecount, aes(Rank, Count, fill = factor(Call_Name))) + 
          geom_bar(stat="identity") + 
          ylab("Message Count") +
          geom_text(data = messagecount, aes(label = Count, y = Count), size = 3, vjust=0) +
          theme_bw() + 
          theme(legend.position = "none") +
          facet_wrap(facets =~ Call_Name, scales = "free_x")
  
  # Data sent/received by function
  sentstat <- output[[4]]
  sentvscallname <- data.frame(Call3=sentstat$Call, Message_size=sentstat$Total)
  Legends3 <- factor(sentvscallname$Call3)
  
  g3 <- ggplot(data=sentvscallname, aes(Call3, Message_size, fill=Legends3)) +
          geom_bar(stat="identity") + 
          xlab("MPI Function") + 
          ylab("Message Size (bytes)") +
          theme_bw() + 
          theme(legend.position="none") +
          geom_text(data=sentvscallname, aes(label=Message_size, y=Message_size), size=3, vjust=0) + 
          theme(axis.text.x=element_text(angle=-30, vjust=0.5))
  
  # Proportion of data sent/received by function
  sentvscallname_per <- data.frame(Call3=sentstat$Call, Message_size_per=sentstat$Sent.)
  Legends4 <- factor(sentvscallname_per$Call3)
  
  g4 <- ggplot(data=sentvscallname_per, aes(Call3, Message_size_per, fill=Legends4)) +
          geom_bar(stat="identity") + 
          xlab("MPI Function") + 
          ylab("Proportion of Message Size") +
          theme_bw() + 
          theme(legend.position="none") +
          geom_text(data=sentvscallname_per, aes(label=Message_size_per, y=Message_size_per), size=3, vjust=0) + 
          theme(axis.text.x=element_text(angle=-30, vjust=0.5))
  
  # Plot a single legend
  tmp <- ggplot_gtable(ggplot_build(g1))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  
  g1 <- g1 + theme(legend.position = "none")
  
  if (1 %in% which)
    add.legend <- TRUE
  
  
  return( list(g1=g1, g2=g2, g3=g3, g4=g4) )
}



### mpip
plot_mpip <- function(x, which=1L:4L, show.title=TRUE, plot.type="timings", label, bar.label=FALSE)
{
  plot.types <- c("timings", "stats1", "stats2", "messages1", "messages2")#, "counts")
  plot.type <- match.arg(tolower(plot.type), plot.types)
  
  add.legend <- FALSE
  
  output <- x@parsed
  
  # --------------------------------------------------------
  # Timing plots
  # --------------------------------------------------------
  
  if (plot.type == "timings")
  {
    plots <- plot_mpip_timing(output=output, bar.label=bar.label, plot.type=plot.type)
    
    if (missing(label))
      label <- "Function Timings"
  }
  
  # --------------------------------------------------------
  # Timing statistics
  # --------------------------------------------------------
  
  else if (plot.type == "stats1" || plot.type == "stats2")
  {
    plots <- plot_mpip_stats(output=output, bar.label=bar.label, plot.type=plot.type)
    
    if (plot.type == "stats1" && missing(label))
      label <- "Timing Statistics by Function"
    
    if (plot.type == "stats2")
    {
      add.legend <- TRUE
      legend <- plots$legend
      plots$legend <- NULL
      
      if (missing(label))
        label <- "Summed Timing Statistics by Rank"
    }
  }
  
  # --------------------------------------------------------
  # Message statistics
  # --------------------------------------------------------
  
  else if (plot.type == "messages1" || plot.type == "messages2")
  {
    plots <- plot_mpip_messages(output=output, bar.label=bar.label, plot.type=plot.type)
    
    if (plot.type == "messages1" && missing(label))
      label <- "Message Statistics by Rank"
    
    if (plot.type == "messages2")
    {
      add.legend <- TRUE
      legend <- plots$legend
      plots$legend <- NULL
      
      if (missing(label))
        label <- "Summed Message Statistics by Rank"
    }
  }
  
  # --------------------------------------------------------
  # Counts ?
  # --------------------------------------------------------
  
  ### FIXME
#  else if (plot.type == "counts")
#  {
#    plots <- plot_mpip_counts(output=output, bar.label=bar.label, plot.type=plot.type)
#    
#    if (missing(label))
#      label <- "Data"
#  }
  
  # --------------------------------------------------------
  # Plot and return
  # --------------------------------------------------------
  
  if (add.legend)
    g <- grid_plotter(plots=plots, which=which, label=label, show.title=show.title, legend=legend)
  else
    g <- grid_plotter(plots=plots, which=which, label=label, show.title=show.title)
  
  return( g )
}
