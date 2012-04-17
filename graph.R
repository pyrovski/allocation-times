#!/usr/bin/env Rscript

d<-read.table("table", header=T)
d$elapsed<-d$stop_time-d$start_time

c1="lightgreen"
c2="darkgreen"
c3="lightblue"
c4="blue"


m1=d$system=="Sierra" & d$time_req=="2m"
m2=d$system=="Sierra" & d$time_req=="5hr"
m3=d$system=="Hera"   & d$time_req=="2m"
m4=d$system=="Hera"   & d$time_req=="5hr"

offset = c(-0.24, -0.08, +0.08, +0.24) 
partial_offset = c(      +0.08, +0.24)
at = c(3+offset, 4+offset, 5+offset, 6+offset, 7+offset, 8+offset, 9+partial_offset, 10+partial_offset)
col=c( rep(c(c1, c2, c3, c4), 6), c1, c2, c1, c2)
     
pdf(file="TimeToStart.pdf")
boxplot( 
	log2(c(d$elapsed[m1], d$elapsed[m2], d$elapsed[m3], d$elapsed[m4]))
	~
	c(log2(d$nodes[m1])+0.01, log2(d$nodes[m2])+0.02, log2(d$nodes[m3])+0.03, log2(d$nodes[m4])+0.04),
	xaxt="n",
#	yaxt="n",
	col=col,
	outline=F,
	at=at,
	xlim=c(2,11),
	xlab="Requested Nodes", 
	ylab="Log2 Seconds to Job Start", main="Time to job start vs. system, requested nodes and job length",
	boxwex=.13
	)

legend("bottomright", legend=c("Sierra, 2-minute job", "Sierra, 5-hour job", "Hera, 2-minute job", "Hera, 5-hour job"), fill=c(c1, c2, c3, c4) )

#axis(side=2, at=seq(0,14,2), labels=10^(seq(0,14,2)-2))
axis(side=1, at=3:10, labels=2^(3:10))
dev.off()
