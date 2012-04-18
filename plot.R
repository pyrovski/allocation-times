#!/usr/bin/env Rscript

octiles = function(input){
  med = c();
  med[4] = median(input)
  med[6] = median(input[input > med[4]])
  med[5] = median(input[input > med[4] & input <= med[6]])
  med[7] = median(input[input > med[6]])
  med[2] = median(input[input <= med[4]])
  med[3] = median(input[input <= med[4] & input > med[2]])
  med[1] = median(input[input <= med[2]])
  return(med);
}

a =read.table('table', header=T);

cat('system\treqTime\tnodes\tmin\tmedian\t1st_oct\t7th_oct\n')

for(system in c('Hera', 'Sierra')){
  for(reqTime in c('5hr', '2m')){
    # split each dataset into two plots
    sel = intersect(which(a$time_req == reqTime), which(a$system == system));
    times = a$stop_time[sel] - a$start_time[sel];

    for(nodes in sort(unique(a$nodes[sel]))){
      # compute median, 1st & 7th octiles
      nsel = intersect(sel, which(a$nodes == nodes))
      ntimes = a$stop_time[nsel] - a$start_time[nsel]
      med = octiles(ntimes)
      minTime = min(ntimes)

      cat(paste(system, reqTime, nodes, minTime, med[4], med[1], med[7], sep='\t'))
      cat('\n')
    }

    #! @todo these should be computed for each node configuration
    meanTime = mean(times);
    sdTime = sd(times);
    include = 2;
    
    sel1 = which(times <= meanTime + include*sdTime);
    sel2 = which(times > meanTime + include*sdTime)

    pdf(paste(system, '_', reqTime, '.pdf', sep=''));
    plot(a$nodes[sel[sel1]], times[sel1], 
	 main=paste(system,'time to allocation for',reqTime,'jobs (log-log)'),
	 ylab='time to allocation (s)', xlab='number of nodes', 
	 xaxt='n', log='xy', 
	 sub=paste('not showing', length(sel2), 'outliers outside', include, 'st. dev.'));
    axis(1, at=unique(a$nodes[sel[sel1]]));
    dev.off();
    
    if(length(sel2) > 0){
      pdf(paste(system, '_', reqTime, '_outliers.pdf', sep=''));
      plot(a$nodes[sel[sel2]], times[sel2], 
	   main=paste(system,'time to allocation for',reqTime,'jobs (outliers) (log-log)'),
	   ylab='time to allocation (s)', 
	   xlab='number of nodes', 
	   xaxt='n', log='xy', sub=paste('showing', length(sel2), 'outliers only'));
      axis(1, at=unique(a$nodes[sel[sel2]]));
      dev.off();
    }
  }
 }
