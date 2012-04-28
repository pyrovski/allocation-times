#!/usr/bin/env Rscript

###########################
# Cost vs Execution times
###########################

vApps = c("BT","CG","EP","LU","SP","Sparse","SMG2000","Sweep3D","LAMMPS","UMT2k")


tmaindata <- read.table("costexec", header=T, sep=";")
tApp=as.matrix(tmaindata$App)

file<-as.character("-cloud")
extension<-as.character(".eps")

linecol = c("grey20","grey40","grey60")

for (napp in 1:10) {
	tdata <- subset(tmaindata, App==napp) 
	tCost=as.vector(tdata$Cost)
	tExec=as.vector(tdata$Exec)
	tType=as.numeric(tdata$Type)
	tTypes=c("CC8","CC","HC")

	filename=""
	filename=paste(vApps[napp],file,extension)
	filename=gsub("\\s","",filename)

	postscript(filename, horizontal=FALSE)

	titletemp="Cost-Execution Time Tradeoff for "
	sTitle=""
	sTitle=paste(titletemp,vApps[napp])

	plot(tExec,
		tCost, 
		xlim = c(0, max(tExec) + 1), 
		ylim = c(0, max(tCost) + 1),
		main = sTitle, 
		xlab = "Execution time (seconds)", 
		ylab = "Cost (cents)", 
		pch = c(19,19,19,15,15,15,17,17,17), 
		col = c("grey10","grey40","grey70","grey10","grey40","grey70","grey10","grey40","grey70"),
		cex = 2, 
		lty = "solid", 
		lwd = 1, 
		xaxs = "i", 
		yaxs = "i")
	
	ntypes = length(tType)
	
	for (i in 1:3) {
		subtdata <- subset(tdata, Type==tTypes[i])
		lines(subtdata$Exec, subtdata$Cost, type="l", col='black',lwd=1)
	}

	par(font.lab=2)
	legend("topleft", 
		title="System Type", 
		inset=.01, 
		"Configuration types", 
		c("CC8","CC","High CPU"), 
		pch = c(19,15,17) )

}

#pdf("temp.pdf")
#
#plot(-4:4, -4:4, type = "n")# setting up coord. system
#points(rnorm(200), rnorm(200), col = "red")
#points(rnorm(100)/2, rnorm(100)/2, col = "blue", cex = 1.5)
#
#op <- par(bg = "light blue")
#x <- seq(0,2*pi, len=51)
### something "between type='b' and type='o'":
#plot(x, sin(x), type="o", pch=21, bg=par("bg"), col = "blue", cex=.6,
# main='plot(..., type="o", pch=21, bg=par("bg"))')
#par(op)
#
###-------- Showing all the extra & some char graphics symbols ------------
#Pex <- 3 ## good for both .Device=="postscript" and "x11"
#ipch <- 1:(np <- 25+11); k <- floor(sqrt(np)); dd <- c(-1,1)/2
#rx <- dd + range(ix <- (ipch-1) %/% k)
#ry <- dd + range(iy <- 3 + (k-1)-(ipch-1) %% k)
#pch <- as.list(ipch)
#pch[25+ 1:11] <- as.list(c("*",".", "o","O","0","+","-",":","|","%","#"))
#plot(rx, ry, type="n", axes = FALSE, xlab = "", ylab = "",
#     main = paste("plot symbols :  points (...  pch = *, cex =", Pex,")"))
#abline(v = ix, h = iy, col = "lightgray", lty = "dotted")
#for(i in 1:np) {
#  pc <- pch[[i]]
#  points(ix[i], iy[i], pch = pc, col = "red", bg = "yellow", cex = Pex)
#  ## red symbols with a yellow interior (where available)
#  text(ix[i] - .3, iy[i], pc, col = "brown", cex = 1.2)
#}


#barplot(btCost, main="System", xlab="TurnaroundTime", ylab="Cost", names.org=btdata$System,
#		border="blue", beside=T, width = 0.4, xlim=max(btTurnaround)
#		, axis.lty=1)
#
#ggplot(x, aes(x = Turnaround, y = Cost, fill=Period)) + scale_x_continuous() + scale_y_continuous() + geom_bar(position=position_dodge())


#barplot(Cost, main = Period,
#	names.arg = Period, #main = Cost,
#        main=paste('Execution + queueing delay for ', appname$App[1]),
#        legend.text = c('Queueing delay', 'Execution'),
#        ylab='log seconds',
#        xlab='System')
#        ylim = c(0, 1.2*(max(log10(mat1))+ max(log10(mat2)))))

