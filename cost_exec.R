#!/usr/bin/env Rscript

###########################
# Cost vs Execution times
###########################

vApps = c("BT","CG","EP","LU","SP","Sparse","SMG2000","Sweep3D","LAMMPS","UMT2k")


tmaindata <- read.table("costexec", header=T, sep=";")
tApp=as.matrix(tmaindata$App)

file<-as.character("-cloud")
extension<-as.character(".eps")

linecol = c("black","grey30","grey60")

max_norm_cost = 0.0
max_norm_exec = 0.0


for (napp in 1:10) {
	tdata1 <- subset(tmaindata, App==napp) 
	tCost=as.vector(tdata1$Cost)
	tExec=as.vector(tdata1$Exec)
	tConfig=as.vector(tdata1$Config)
	if(max_norm_cost < max(tCost/tCost[1])) {
		max_norm_cost = max(tCost/tCost[1])
	}
	
	if(max_norm_exec < max(tExec/tExec[1])) {
		max_norm_exec = max(tExec/tExec[1])
	}
}

for (napp in 1:10) {
	tdata <- subset(tmaindata, App==napp) 
	tCost=as.vector(tdata$Cost)
	tExec=as.vector(tdata$Exec)
	tType=as.numeric(tdata$Type)
	tTypes=c("CC2","CC1","HC")

	filename=""
	filename=paste(vApps[napp],file,extension)
	filename=gsub("\\s","",filename)
	postscript(filename, horizontal=FALSE)

	titletemp="Cost-Execution Time Tradeoff for "
	sTitle=""
	sTitle=paste(titletemp,vApps[napp])
	
	norm_cost = tCost[1]	
	norm_time = tExec[1]

	plot(tCost/norm_cost,
		tExec/norm_time, 
		xlim = c(0, max_norm_cost+1), 
		ylim = c(0, max_norm_exec+1),
		main = sTitle, 
		xlab = "Normalized Cost", 
		ylab = "Normalized Execution Time", 
		pch = c(16,16,16,15,15,15,17,17,17), 
		col = c("black","grey30","grey60","black","grey30","grey60","black","grey30","grey60"),
		cex = 2, 
		lty = "solid", 
		lwd = 1, 
		xaxs = "i", 
		yaxs = "i")
	
	ntypes = length(tType)
	abline(h=c(1.0), col="gray")	
	abline(v=c(1.0), col="gray")	
	for (i in 1:3) {
		subtdata <- subset(tdata, Type==tTypes[i])
		lines(subtdata$Cost/norm_cost, subtdata$Exec/norm_time, type="l", col='black',lwd=1)
	}

	

	par(font.lab=2)
	legend("topleft", 
		title="System Type", 
		inset=.01, 
		"Configuration types", 
		c(tConfig[1],
		  tConfig[2],
		  tConfig[3],
		  tConfig[4],
		  tConfig[5],
		  tConfig[6],
		  tConfig[7],
		  tConfig[8],
		  tConfig[9]

		), 
		pch = c(16,16,16,15,15,15,17,17,17),
		col = c("black","grey30","grey60","black","grey30","grey60","black","grey30","grey60"))


}


