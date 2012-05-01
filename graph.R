d<-read.table("table.txt", header=T)
d$elapsed<-d$stop_time-d$start_time

#c1="lightgreen"
#c2="darkgreen"
#c3="lightblue"
#c4="blue"
#
#
#m1=d$system=="Sierra" & d$time_req=="2m"  & d$nodes==1024
#m2=d$system=="Sierra" & d$time_req=="5hr" & d$nodes==1024
#m3=d$system=="Hera"   & d$time_req=="2m"  & d$nodes==1024
#m4=d$system=="Hera"   & d$time_req=="5hr" & d$nodes==1024
#
#offset = c(-0.24, -0.08, +0.08, +0.24) 
#partial_offset = c(      +0.08, +0.24)
#at = c(3+offset, 4+offset, 5+offset, 6+offset, 7+offset, 8+offset, 9+partial_offset, 10+partial_offset)
#col=c( rep(c(c1, c2, c3, c4), 6), c1, c2, c1, c2)
     
#pdf(file="TimeToStart.pdf")
#boxplot( 
#	log2(c(d$elapsed[m1], d$elapsed[m2], d$elapsed[m3], d$elapsed[m4]))
#	~
#	c(log2(d$nodes[m1])+0.01, log2(d$nodes[m2])+0.02, log2(d$nodes[m3])+0.03, log2(d$nodes[m4])+0.04),
#	xaxt="n",
#	col=col,
#	outline=F,
#	at=at,
#	xlim=c(2,11),
#	xlab="Log2 Requested Nodes", ylab="Log2 Seconds to Job Start", main="Time to job start vs. system, requested nodes and job length",
#	boxwex=.13
#	)
#
#legend("bottomright", legend=c("Sierra, 2-minute job", "Sierra, 5-hour job", "Hera, 2-minute job", "Hera, 5-hour job"), fill=c(c1, c2, c3, c4) )
#
#axis(side=1, at=3:10)
#dev.off()


#
#
# Sierra 128 nodes 1024 tasks
#
#



pdf(file="sierra_128_1024.pdf", width=6, height=6)

m1=d$system=="Sierra" & d$time_req=="2m"  & d$nodes==128
m2=d$system=="Sierra" & d$time_req=="5hr" & d$nodes==128
m3=d$system=="Hera"   & d$time_req=="2m"  & d$nodes==128
m4=d$system=="Hera"   & d$time_req=="5hr" & d$nodes==128

s<-read.table("data2.R", header=T)
sierra_1024_speedup <- c( s$t[s$m=="S" & s$n==1024] / s$t[s$m=="C" & s$n==1024] )

xaxis<-barplot( c(0, 0, sierra_1024_speedup), ylim=c(0,3.7), yaxt="n", xaxt="n" )

lines( x=c(0,100), y=c(1,1) )


fiveH<-60*60*5
twoM<-60*2
offset=.2
lwd=1.5
width=1.0

##############################################################################
# Pure turnaround
boxplot(xaxt="n", yaxt="n", lwd=lwd,
	c(d$elapsed[m2]/fiveH),
	add=T,
	col="darkgreen",
	boxwex=width,
	outline=F,
	at=xaxis[1]
	)

##############################################################################
# Benchmark 1-5
for (i in 1:9){
boxplot(xaxt="n", yaxt="n", lwd=lwd, 
	c( ((d$elapsed[m2]+(fiveH*sierra_1024_speedup[i]))/fiveH) ),
	add=T,
	col="darkgreen",
	boxwex=width,
	outline=F,
	at=xaxis[i+2]
	)
}
##############################################################################

axis(side=2, at=c(1), labels=c("5 hours"), cex.axis=0.8)
axis(side=1, at=xaxis, labels=c("(none)", "", "CG", "EP", "Sweep3D", "LU", "SP", "BT", "SM", "SPM", "LMP"), cex.axis=0.6 )

legend(x="topleft", fill=c("gray", "darkgreen" ), cex=0.7,
	legend=c(
		"1024-task Sierra execution time normalized to 5-hour Amazon CC2 job", 
		"Distribution of turnaround time assuming 5-hour Sierra job queue time"
	) 
)

dev.off()

#
#
# Hera 128 nodes 1024 tasks
#
#



pdf(file="hera_128_1024.pdf", width=6, height=6)

m1=d$system=="Sierra" & d$time_req=="2m"  & d$nodes==128
m2=d$system=="Sierra" & d$time_req=="5hr" & d$nodes==128
m3=d$system=="Hera"   & d$time_req=="2m"  & d$nodes==128
m4=d$system=="Hera"   & d$time_req=="5hr" & d$nodes==128

s<-read.table("data2.R", header=T)
hera_1024_speedup <- c( s$t[s$m=="H" & s$n==1024] / s$t[s$m=="C" & s$n==1024] )

xaxis<-barplot( c(0, 0, hera_1024_speedup), ylim=c(0,3.7), yaxt="n", xaxt="n" )

lines( x=c(0,100), y=c(1,1) )


fiveH<-60*60*5
twoM<-60*2
offset=.2
lwd=1.5
width=1.0

##############################################################################
# Pure turnaround
boxplot(xaxt="n", yaxt="n", lwd=lwd,
	c(d$elapsed[m4]/fiveH),
	add=T,
	col="blue",
	boxwex=width,
	outline=F,
	at=xaxis[1]
	)

##############################################################################
# Benchmark 1-5
for (i in 1:9){
boxplot(xaxt="n", yaxt="n", lwd=lwd, 
	c( ((d$elapsed[m4]+(fiveH*hera_1024_speedup[i]))/fiveH) ),
	add=T,
	col="blue",
	boxwex=width,
	outline=F,
	at=xaxis[i+2]
	)
}
##############################################################################

axis(side=2, at=c(1), labels=c("5 hours"), cex.axis=0.8)
axis(side=1, at=xaxis, labels=c("(none)", "", "CG", "EP", "Sweep3D", "LU", "SP", "BT", "SM", "SPM", "LMP"), cex.axis=0.6 )

legend(x="topleft", fill=c("gray", "blue" ), cex=0.7,
	legend=c(
		"1024-task Hera execution time normalized to 5-hour Amazon CC2 job", 
		"Distribution of turnaround time assuming 5-hour Hera job queue time"
	) 
)

dev.off()



############################################################################
# 64 nodes


#
#
# Sierra 64 nodes 512 tasks
#
#


d<-read.table("table.txt", header=T)
d$elapsed<-d$stop_time-d$start_time

pdf(file="sierra_64_512.pdf", width=6, height=6)

m1=d$system=="Sierra" & d$time_req=="2m"  & d$nodes==64
m2=d$system=="Sierra" & d$time_req=="5hr" & d$nodes==64
m3=d$system=="Hera"   & d$time_req=="2m"  & d$nodes==64
m4=d$system=="Hera"   & d$time_req=="5hr" & d$nodes==64

s<-read.table("data2.R", header=T)
sierra_512_speedup <- c( s$t[s$m=="S" & s$n==512] / s$t[s$m=="C" & s$n==512] )

xaxis<-barplot( c(0, 0, sierra_512_speedup), ylim=c(0,3.7), yaxt="n", xaxt="n" )

lines( x=c(0,100), y=c(1,1) )


fiveH<-60*60*5
twoM<-60*2
offset=.2
lwd=1.5
width=1.0

##############################################################################
# Pure turnaround
boxplot(xaxt="n", yaxt="n", lwd=lwd,
	c(d$elapsed[m2]/fiveH),
	add=T,
	col="darkgreen",
	boxwex=width,
	outline=F,
	at=xaxis[1]
	)

##############################################################################
# Benchmark 1-5
for (i in 1:5){
boxplot(xaxt="n", yaxt="n", lwd=lwd, 
	c( ((d$elapsed[m2]+(fiveH*sierra_512_speedup[i]))/fiveH) ),
	add=T,
	col="darkgreen",
	boxwex=width,
	outline=F,
	at=xaxis[i+2]
	)
}
##############################################################################

axis(side=2, at=c(1), labels=c("5 hours"), cex.axis=0.8)
axis(side=1, at=xaxis, labels=c("(none)", "", "CG", "EP", "Sweep3D", "LU", "SP"), cex.axis=0.6 )

legend(x="topleft", fill=c("gray", "darkgreen" ), cex=0.7,
	legend=c(
		"512-task Sierra execution time normalized to 5-hour Amazon CC2 job", 
		"Distribution of turnaround time assuming 5-hour Sierra job queue time"
	) 
)

dev.off()

#
#
# Hera 64 nodes 512 tasks
#
#



pdf(file="hera_64_512.pdf", width=6, height=6)

m1=d$system=="Sierra" & d$time_req=="2m"  & d$nodes==64
m2=d$system=="Sierra" & d$time_req=="5hr" & d$nodes==64
m3=d$system=="Hera"   & d$time_req=="2m"  & d$nodes==64
m4=d$system=="Hera"   & d$time_req=="5hr" & d$nodes==64

s<-read.table("data2.R", header=T)
hera_512_speedup <- c( s$t[s$m=="H" & s$n==512] / s$t[s$m=="C" & s$n==512] )

xaxis<-barplot( c(0, 0, hera_512_speedup), ylim=c(0,3.7), yaxt="n", xaxt="n" )

lines( x=c(0,100), y=c(1,1) )


fiveH<-60*60*5
twoM<-60*2
offset=.2
lwd=1.5
width=1.0

##############################################################################
# Pure turnaround
boxplot(xaxt="n", yaxt="n", lwd=lwd,
	c(d$elapsed[m4]/fiveH),
	add=T,
	col="blue",
	boxwex=width,
	outline=F,
	at=xaxis[1]
	)

##############################################################################
# Benchmark 1-5
for (i in 1:5){
boxplot(xaxt="n", yaxt="n", lwd=lwd, 
	c( ((d$elapsed[m4]+(fiveH*hera_512_speedup[i]))/fiveH) ),
	add=T,
	col="blue",
	boxwex=width,
	outline=F,
	at=xaxis[i+2]
	)
}
##############################################################################

axis(side=2, at=c(1), labels=c("5 hours"), cex.axis=0.8)
axis(side=1, at=xaxis, labels=c("(none)", "", "CG", "EP", "Sweep3D", "LU", "SP"), cex.axis=0.6 )

legend(x="topleft", fill=c("gray", "blue" ), cex=0.7,
	legend=c(
		"512-task Hera execution time normalized to 5-hour Amazon CC2 job", 
		"Distribution of turnaround time assuming 5-hour Hera job queue time"
	) 
)

dev.off()




############################################################################
# 32 nodes


#
#
# Sierra 32 nodes 256 tasks
#
#


d<-read.table("table.txt", header=T)
d$elapsed<-d$stop_time-d$start_time

pdf(file="sierra_32_256.pdf", width=12, height=6)

m1=d$system=="Sierra" & d$time_req=="2m"  & d$nodes==32
m2=d$system=="Sierra" & d$time_req=="5hr" & d$nodes==32
m3=d$system=="Hera"   & d$time_req=="2m"  & d$nodes==32
m4=d$system=="Hera"   & d$time_req=="5hr" & d$nodes==32

s<-read.table("data2.R", header=T)
sierra_256_speedup <- c( s$t[s$m=="S" & s$n==256] / s$t[s$m=="C" & s$n==256] )


#lines( x=c(0,100), y=c(1,1) )


fiveH<-60*60*5
twoM<-60*2
offset=.2
lwd=1.5
width=1.0

###############################################################################
## Pure turnaround
#boxplot(xaxt="n", yaxt="n", lwd=lwd,
#	c(d$elapsed[m2]/fiveH),
#	add=T,
#	col="darkgreen",
#	boxwex=width,
#	outline=F,
#	at=xaxis[1]
#	)
#
###############################################################################
## Benchmark 1-5
#for (i in 1:5){
#boxplot(xaxt="n", yaxt="n", lwd=lwd, 
#	c( ((d$elapsed[m2]+(fiveH*sierra_256_speedup[i]))/fiveH) ),
#	add=T,
#	col="darkgreen",
#	boxwex=width,
#	outline=F,
#	at=xaxis[i+2]
#	)
#}
###############################################################################
#
#axis(side=2, at=c(1), labels=c("5 hours"), cex.axis=0.8)
#axis(side=1, at=xaxis, labels=c("(none)", "", "CG", "EP", "Sweep3D", "LU", "SP"), cex.axis=0.6 )
#
#legend(x="topleft", fill=c("gray", "darkgreen" ), cex=0.7,
#	legend=c(
#		"256-task Sierra execution time normalized to 5-hour Amazon CC2 job", 
#		"Distribution of turnaround time assuming 5-hour Sierra job queue time"
#	) 
#)
#
#dev.off()

#
#
# Hera 32 nodes 256 tasks
#
#

hera_256_speedup <- c( s$t[s$m=="H" & s$n==256] / s$t[s$m=="C" & s$n==256] )

sierradata <- as.vector(c(0,sierra_256_speedup))
heradata <- as.vector(c(0,hera_256_speedup))

combined<-matrix(nrow=2, ncol=6, c(0,sierra_256_speedup, 0, hera_256_speedup), byrow=T)

xaxis<-barplot( combined, ylim=c(0,3.7), yaxt="n", xaxt="n", beside=TRUE)
#xaxis<-barplot( c(sierradata, heradata), ylim=c(0,3.7), yaxt="n", xaxt="n", beside=TRUE)
#xaxis<-barplot( plotdata, ylim=c(0,3.7), yaxt="n", xaxt="n", beside=TRUE)
#xaxis<-barplot( c(sierra_256_speedup, hera_256_speedup), ylim=c(0,3.7), yaxt="n", xaxt="n", beside=TRUE)

lines( x=c(0,100), y=c(1,1) )

fiveH<-60*60*5
twoM<-60*2
offset=.2
lwd=1.5
width=1.0

##############################################################################
# Pure turnaround
boxplot(xaxt="n", yaxt="n", lwd=lwd,
	c(d$elapsed[m2]/fiveH),
	add=T,
	col="darkgreen",
	boxwex=width,
	outline=F,
	at=xaxis[1]
	)

boxplot(xaxt="n", yaxt="n", lwd=lwd,
	c(d$elapsed[m4]/fiveH),
	add=T,
	col="blue",
	boxwex=width,
	outline=F,
	at=xaxis[2]
	)

##############################################################################
# Benchmark 1-5
for (i in 1:5){
boxplot(xaxt="n", yaxt="n", lwd=lwd, 
	c((d$elapsed[m2]+(fiveH*sierra_256_speedup[i]))/fiveH),
	add=T,
	col="darkgreen",
	boxwex=width,
	outline=F,
	at=xaxis[i*2+1]
	)
}

for (i in 1:5){
boxplot(xaxt="n", yaxt="n", lwd=lwd, 
	c((d$elapsed[m4]+(fiveH*hera_256_speedup[i]))/fiveH),
	add=T,
	col="blue",
	boxwex=width,
	outline=F,
	at=xaxis[2*i+2]
	)
}

##############################################################################

axis(side=2, at=c(1), labels=c("5 hours"), cex.axis=0.8)



#axis(side=1, at=xaxis, labels=c("(none)", "CG", "EP", "Sweep3D", "LU", "SP"), cex.axis=0.6 )

legend(x="topleft", fill=c("gray", "darkgreen", "blue" ), cex=0.7,
	legend=c(
		"256-task Sierra execution time normalized to 5-hour Amazon CC2 job", 
		"256-task Hera execution time normalized to 5-hour Amazon CC2 job", 
		"Distribution of turnaround time assuming 5-hour Hera job queue time"
	) 
)

dev.off()


