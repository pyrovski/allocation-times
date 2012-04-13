#!/usr/bin/env Rscript

a =read.table('table', header=T);

for(reqTime in c('5hr', '2m')){
  for(system in c('Hera', 'Sierra')){
    pdf(paste(system, '_', reqTime, '.pdf', sep=''));
    sel = intersect(which(a$time_req == reqTime), which(a$system == system));
    times = a$stop_time[sel] - a$start_time[sel];
    plot(a$nodes[sel], times, main=paste(system,'time to allocation for',reqTime,'jobs'),ylab='time to allocation (s)', xlab='number of nodes', xaxt='n'); 
    axis(1, at=unique(a$nodes[sel]));
    dev.off();
  }
 }
