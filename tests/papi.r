library(scribe)
library(ggplot2)

file <- system.file("testdata/cache_misses.Rda", package="scribe")
load(file)

plot(cm_1e4)

plot(cm_1e4, cm_5e4, cm_1e5)

plot(cm_1e4, cm_5e4, cm_1e5, color=TRUE, show.opnames=FALSE)

plot(cm_1e4, cm_5e4, cm_1e5, facet.by="level") + 
    theme(axis.text.x=element_text(angle=30, hjust=1))

plot(cm_1e4, cm_5e4, cm_1e5, color=TRUE, show.opnames=FALSE, facet.by="level")
