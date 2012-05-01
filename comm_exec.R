#!/usr/bin/env Rscript

library(ggplot2)
library(reshape2)

vApps = c("BT","CG","EP","LU","SP","Sparse","SMG2000","Sweep3D","LAMMPS")

w = read.table('comm_exec.data', sep=';', header=T)

App=as.matrix(w$App)
System=as.matrix(w$System)
Time=as.matrix(w$Time)
Component=as.matrix(w$Component)
BMType=as.matrix(w$BMType)
nTime=

w$App <- factor(w$App, levels(w$App)[c(1,2,3,5,7,8,6,9,4)])


for(i in 1:9) {

	appdata <- subset(w, vApps[i]==w$App)
	cc2data <- subset(appdata, appdata$System=="CC2")
	cc2comp <- cc2data$Time[1]
	cc2comm <- cc2data$Time[2]
	cc2exec = cc2comp + cc2comm
	ncc2comp = cc2comp/cc2exec
	ncc2comm = cc2comm/cc2exec

	sierradata <- subset(appdata, appdata$System=="Sierra")
	sierracomp <- sierradata$Time[1]
	sierracomm <- sierradata$Time[2]
	sierraexec = sierracomp + sierracomm
	nsierraexec = sierraexec/cc2exec
	nsierracomp = nsierraexec * (sierracomp/sierraexec)
	nsierracomm = nsierraexec * (sierracomm/sierraexec)

	heradata <- subset(appdata, appdata$System=="Hera")
	heracomp <- heradata$Time[1]
	heracomm <- heradata$Time[2]
	heraexec = heracomp + heracomm
	nheraexec = heraexec/cc2exec
	nheracomp = nheraexec * (heracomp/heraexec)
	nheracomm = nheraexec * (heracomm/heraexec)

	append(nTime, as.matrix(c(ncc2comp,ncc2comm,nsierracomp,nsierracomm,nheracomp,nheracomm)))
	w$Time[w$Time == cc2comp   ] = ncc2comp 
	w$Time[w$Time == cc2comm   ] = ncc2comm
	w$Time[w$Time == sierracomp] = nsierracomp 
	w$Time[w$Time == sierracomm] = nsierracomm
	w$Time[w$Time == heracomp  ] = nheracomp 
	w$Time[w$Time == heracomm  ] = nheracomm


	cat("App:", vApps[i], "CC2:",cc2comp,cc2comm,"Normalized:",ncc2comp, ncc2comm,"\n")	
	cat("\tSierra:",sierracomp,sierracomm,"Normalized:",nsierracomp, nsierracomm,"\n")	

}

#nw <- data.frame(BMType,App,System,nTime,Component)


 ggplot(w, aes(x=System, y=Time, fill=Component, order = -as.numeric(Component)), yaxs = "i") +
    ylab("Normalized Execution Time to CC2") + 
    xlab("Systems") +
    facet_grid(. ~ App, scales="free") +
    scale_fill_manual(values = c('white','grey20')) +
    scale_y_continuous(limits=c(0,2))+
    opts(title = 'Normalized Application Execution Times') +
    opts(fontsize = 5) + 
    opts(legend.position = 'top') +
    geom_bar() + 
    geom_bar(colour = 'black', show_guide=FALSE) +
    opts(legend.key = theme_rect(colour = 'black')) +
    labs(fill="") +
    opts(axis.title.x = theme_text(vjust=-0.5))
 #   scale_fill_discrete("")
    
 
 ggsave(filename=paste("execplot", "eps", sep='.'), width=15, height=6)
