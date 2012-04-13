#!/usr/bin/env Rscript

a =read.table('table', header=T);

######################################################################
sel = intersect(which(a$time_req == '5hr'), which(a$system == 'Hera'));

times = a$stop_time[sel] - a$start_time[sel];

pdf('hera_5hr.pdf');
plot(a$nodes[sel], times, main=paste('Hera','time to allocation for','5hr','jobs'),ylab='time to allocation (s)', xlab='number of nodes', xaxt='n'); 
axis(1, at=unique(a$nodes[sel]));
dev.off();

######################################################################sel = intersect(which(a$time_req == '2m'), which(a$system == 'Hera'));

sel = intersect(which(a$time_req == '2m'), which(a$system == 'Hera'));
times = a$stop_time[sel] - a$start_time[sel];

pdf('hera_2m.pdf');
plot(a$nodes[sel], times, main=paste('Hera','time to allocation for','2m','jobs'),ylab='time to allocation (s)', xlab='number of nodes', xaxt='n'); 
axis(1, at=unique(a$nodes[sel]));
dev.off();

######################################################################

sel = intersect(which(a$time_req == '5hr'), which(a$system == 'Sierra'));
times = a$stop_time[sel] - a$start_time[sel];

pdf('Sierra_5hr.pdf');
plot(a$nodes[sel], times, main=paste('Hera','time to allocation for','5hr','jobs'),ylab='time to allocation (s)', xlab='number of nodes', xaxt='n'); 
axis(1, at=unique(a$nodes[sel]));
dev.off();

######################################################################

sel = intersect(which(a$time_req == '2m'), which(a$system == 'Sierra'));
times = a$stop_time[sel] - a$start_time[sel];

pdf('Sierra_2m.pdf');
plot(a$nodes[sel], times, main=paste('Hera','time to allocation for','2m','jobs'),ylab='time to allocation (s)', xlab='number of nodes', xaxt='n'); 
axis(1, at=unique(a$nodes[sel]));
dev.off();
