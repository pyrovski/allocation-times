#!/usr/bin/env Rscript

library(ggplot2)
library(reshape2)

# merge tables

w = read.table('comm_exec.data', sep=';', header=T)

App=as.matrix(w$App)
System=as.matrix(w$System)
Time=as.matrix(w$Time)
Component=as.matrix(w$Component)
BMType=as.matrix(w$BMType)

w$App <- factor(w$App, levels(w$App)[c(1,2,3,5,7,8,6,9,4)])

# want a data frame that has one column for exec and one for queueing,
# with an id column for 'w', 'm', or 'b' for 'worst', 'median', and 'best',
# respectively.

 ggplot(w, aes(x=System, y=Time, fill=Component, order = -as.numeric(Component)), yaxs = "i") +
    ylab("Execution Time") + 
    xlab("Systems") +
    facet_grid(. ~ App, scales="free") +
    scale_fill_manual(values = c('white','grey20')) +
    opts(title = 'Application Execution Times ') +
    opts(fontsize = 5) + 
    opts(legend.position = 'top') +
    geom_bar() + 
    geom_bar(colour = 'black', show_guide=FALSE) +
    opts(legend.key = theme_rect(colour = 'black')) 
 ggsave(filename=paste("execplot", "eps", sep='.'), width=15, height=6)
