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

e1 = d$elapsed[m1]
e2 = d$elapsed[m2]
e3 = d$elapsed[m3]
e4 = d$elapsed[m4]

m1sel = which(e1 < mean(e1) + 2*sd(e1))
m2sel = which(e2 < mean(e2) + 2*sd(e2))
m3sel = which(e3 < mean(e3) + 2*sd(e3))
m4sel = which(e4 < mean(e4) + 2*sd(e4))

e1 = e1[m1sel]
e2 = e2[m2sel]
e3 = e3[m3sel]
e4 = e4[m4sel]

n1 = d$nodes[m1][m1sel]
n2 = d$nodes[m2][m2sel]
n3 = d$nodes[m3][m3sel]
n4 = d$nodes[m4][m4sel]

offset = c(-0.24, -0.08, +0.08, +0.24) 
partial_offset = c(      +0.08, +0.24)
at = c(3+offset, 4+offset, 5+offset, 6+offset, 7+offset, 8+offset, 9+partial_offset, 10+partial_offset)
col=c( rep(c(c1, c2, c3, c4), 6), c1, c2, c1, c2)
     
#pdf(file="TimeToStart_no_outliers.pdf")
postscript(file='TimeToStart_no_outliers.eps')
boxplot( 
	log10(c(e1, e2, e3, e4))
	~
	c(log2(n1)+0.01, log2(n2)+0.02, log2(n3)+0.03, log2(n4)+0.04),
	xaxt="n",
	yaxt="n",
	col=col,
	outline=F,
	at=at,
	xlim=c(2,11),
	ylim=c(0,5),
	xlab="Requested Nodes", 
	ylab="Seconds to Job Start", main="Time to job start vs. system, requested nodes and job length",
	boxwex=.13
	)

legend("bottomright", legend=c("Sierra, 2-minute job", "Sierra, 5-hour job", "Hera, 2-minute job", "Hera, 5-hour job"), fill=c(c1, c2, c3, c4) )

axis(side=2, at=0:5, labels=10^(0:5))
axis(side=1, at=3:10, labels=2^(3:10))
dev.off()
