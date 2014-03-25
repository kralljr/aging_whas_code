# setwd("/Users/jennakrall/Dropbox/Aging/dat_16feb12_impute")



library(MplusAutomation)





getgrowth <- function(name1){
	par1 <- extractModelParameters(name1)
	parcheck1 <- fixdat(par1)
	Is <- c("IX", "IY")
	Ss <- c("SX", "SY")
	wh <- which((parcheck1$onw == "ON" & parcheck1$param %in% Is &
		 parcheck1$var %in% Ss) | parcheck1$onw == "WITH")
	out <- parcheck1[wh, ]
}


getgrowth2 <- function(fp, name1) {
	
  par1 <- extractModelParameters(file.path(fp, name1))
  parcheck1 <- fixdat(par1)

  wh <- which(parcheck1$paramHeader =="Intercepts"  &
                parcheck1$param %in% c("I", "S"))
  out <- parcheck1[wh, ]
	out
}



plotgrowth <- function(out, phys, cog) {
	par(mar = c(7,4,4,2))
	
	lb <- (out[,3] - 1.96 * out[,4])
	ub <- (out[,3] + 1.96 * out[,4])
	plot(seq(1:nrow(out)), out[,3], 
		ylim = c(min(lb), max(ub)), pch = 16,
		ylab = "Std est +- 2se", main = paste(cog, phys), 
		xlab = "",
		axes = F, xlim = c(0.5, 6))
	axis(2)
	box()
	# axis(1, at = seq(1,6), labels = paste(out[,1], out[,2]),
		# las = 2)
	labs <- vector(, length = 6)
	labs[1] <- paste(cog, "on", phys)
	labs[2] <- paste(phys, "on", cog)
	labs[3] <- paste(cog,":", "s with int", sep = "")
	labs[4] <- paste(phys,":", "s with int", sep = "")
	labs[5] <- paste("corr int")
	labs[6] <- paste("corr slope")
	axis(1, at = seq(1,6), labels = labs, las = 2)	
	
	abline( h = 0 , col = "grey70", lty = 2)
	abline( v= 2.5, col = "red")
	segments(seq(1,6), lb, seq(1,6), ub)
	
	mtext("Regress slope_y on int_x", at = 1.2)
	
	
}


#function to extract data
fixdat <- function(parname, std=TRUE){
	
	#use standardized parameters?
	if(std!=TRUE){
		pars <- parname[["unstandardized"]]
	}else{
		pars <- parname[["stdyx.standardized"]]
	}
	
	
	#substr names
	names <- strsplit(pars[,1],"\\.")
	namesadd <- matrix(nrow=nrow(pars),ncol=2)
	for(i in 1:length(names)){
		namesadd[i,1] <- names[[i]][1]
		if(length(names[[i]])>1){
			namesadd[i,2] <- names[[i]][2]
		}
	}
	cn <- colnames(pars)
	pars <- data.frame(pars,namesadd)
	colnames(pars) <- c(cn,"var","onw")
	pars
	
}




# parcheck <- fixdat(sppbpar)




#to extract autoreg info, type = "SP"/"TB"/"WS"
regs <- function(pars,typey,typex){
	whr <- which(substr(pars$var,1,2)==typey & substr(pars$param,1,2)==typex &
		pars$onw=="ON"  )
#	ests <- pars[whr,c("est","se","pval")]
	ests <- pars[whr,]
	
	# x <- nchar(ests$param)
	# if(is.numeric(substr(ests$param,x,x))==TRUE){
		# nums <- as.numeric(substr(ests$param,x,x))
	# }else{
		x <- nchar(as.character(ests$var))
		nums <-  as.numeric(substr(as.character(ests$var),x,x))
		
	# }
	ests <- ests[order(nums),]
	ests
}



# # check <- regs(parcheck,"SP","TB")
# check <- regs(parcheck,"TB","SP")
# regs(parcheck,"TB","TB")
# regs(parcheck,"SP","SP")
# regs(parcheck,"SP","TA")
# regs(parcheck,"TB","TA")
# regs(parcheck,"TB","AG")
# regs(parcheck,"TB","RC")







#outer plot
pointsfun <- function(pars,typey,typex,eps,pchs=16,cols=1){
	ests <- regs(pars,typey,typex)
	lb <- ests$est-1.96*ests$se
	ub <- ests$est+1.96*ests$se
	
	points(seq(1+eps,nrow(ests)+eps),ests$est,pch=pchs,
		col=cols)
	abline(h=0,col="grey70")
	for(i in 1:nrow(ests)){
		# est <- ests$est[i]
		# se <- ests$se[i]
		segments(i+eps,lb[i],i+eps,ub[i],col=cols)
		
	}

}




plotfun <- function(pars,mainname,typey,typex,pchs=16,
		cols=1,sp1=FALSE){
	ests <- regs(pars,typey,typex)
	lb <- ests$est-1.96*ests$se
	ub <- ests$est+1.96*ests$se
	
	ylims <- c(min(-.2,min(lb)),max(.2,max(ub)))
	xlims <- c(0,nrow(ests)+1)
	
	plot(seq(1,nrow(ests)),ests$est,xaxt="n",pch=pchs,
		col=cols,main=mainname,ylim=ylims,xlab="Outcome visit",
		ylab="estimate",xlim=xlims)
	
	if(sp1==FALSE){
		# axis(1,at=seq(1,nrow(ests)),labels=ests$var)
		
		axis(1,at=seq(1,nrow(ests)),labels=seq(2,nrow(ests)+1))
	}else if(nrow(ests)==4){
		 # axis(1,at=seq(1,nrow(ests)),
			 # labels=c(as.character(ests$var[c(1:3,5)]),
			 # paste(substr(ests$var[1],1,2),"6",sep="")))
			axis(1,at=seq(1,nrow(ests)),
			 labels=c(2:3,5:6))
			 
	}else{
		if(nrow(ests)==3){
			axis(1,at=seq(1,nrow(ests)),
				labels=c(1:2,4))
		}else{
			axis(1)
			}
		}
	abline(h=0,col="grey70")
	for(i in 1:nrow(ests)){
		# est <- ests$est[i]
		# se <- ests$se[i]
		segments(i,lb[i],i,ub[i],col=cols)
		
	}

}







#crosslag and autoregressive plots plots
plotsppb <- function(name,pdfname){
	par <- extractModelParameters(name)
	parcheck <- fixdat(par)
	
	pdf(pdfname)
	par(mfrow=c(2,2))
	plotfun(parcheck,"Crosslag effects of SPPB on Trails B","TB","SP",sp1=TRUE)
	plotfun(parcheck,"Crosslag effects of Trails B on SPPB","SP","TB",sp1=TRUE)
	plotfun(parcheck,"Autoregressive effects of SPPB","SP","SP",sp1=TRUE)
	plotfun(parcheck,"Autoregressive effects of Trails B","TB","TB",sp1=TRUE)
	graphics.off()
	}
	
	
plotws <- function(name,pdfname){
	par <- extractModelParameters(name)
	parcheck <- fixdat(par)
	
	pdf(pdfname)
	par(mfrow=c(2,2))
	plotfun(parcheck,"Crosslag effects of WS on Trails B","TB","WS")
	plotfun(parcheck,"Crosslag effects of Trails B on WS","WS","TB")
	plotfun(parcheck,"Autoregressive effects of WS","WS","WS")
	plotfun(parcheck,"Autoregressive effects of Trails B","TB","TB")
	graphics.off()
	}
	
	
plottaws <- function(name,pdfname){
		par <- extractModelParameters(name)
	parcheck <- fixdat(par)
	
	pdf(pdfname,height=7,width=11)
	par(mfrow=c(1,2))
	plotfun(parcheck,"Cross Sectional Effects of Trails A on WS","WS","TA")
	plotfun(parcheck,"Cross Sectional Effects of Trails A on Trails B","TB","TA")
	graphics.off()
}



plottasp <- function(name,pdfname){
		par <- extractModelParameters(name)
	parcheck <- fixdat(par)
	
	pdf(pdfname,height=7,width=11)
	par(mfrow=c(1,2))
	plotfun(parcheck,"Cross Sectional Effects of Trails A on SPPB","SP","TA")
	plotfun(parcheck,"Cross Sectional Effects of Trails A on Trails B","TB","TA")
	graphics.off()
}












##### sens
plotoutsensTA <- function(names,pdfname="no",
	mainname,legs, var1,
	var2="TA",var3,sp=FALSE,
	pdf=TRUE,ta=TRUE){
	
	if(length(var2)==1){
		var2 <- matrix(rep(var2,2*length(names)),ncol=2)
		
	}
	par <- extractModelParameters(names[[1]])
	parcheck <- fixdat(par)
	
	if(pdf==TRUE){
		pdf(pdfname,height=7,width=11)
	}
	par(mfrow=c(1,2))
	# plotfun(parcheck,pdfname[[1]],"SP","TA")
	plotfun(parcheck,mainname[[1]],var1,var2[1,1],sp1=sp)
	
	sq <- seq(0,.5,length=length(names))
	colme <- seq(1,length(names))
	pchme <- seq(1,length(names))
	
	for(i in 2:length(names)){
		partemp <- extractModelParameters(names[[i]])
		partemp <- fixdat(partemp)
		pointsfun(partemp,var1,var2[i,1],eps=sq[i],
			cols=colme[i],pchs=pchme[i])
		
	}
	
	
	plotfun(parcheck,mainname[[2]],var3,var2[1,2],sp1=sp)
	
	
	sq <- seq(0,.5,length=length(names))
	colme <- seq(1,length(names))
	pchme <- seq(1,length(names))
	
	for(i in 2:length(names)){
		partemp <- extractModelParameters(names[[i]])
		partemp <- fixdat(partemp)
	#	print(list(partemp,var2[i,2]))
		pointsfun(partemp,var3,var2[i,2],eps=sq[i],
			cols=colme[i],pchs=pchme[i])
		
	}
	legend("bottomright",legend=legs,pch=pchme,
		col=colme,lty=1)
	if(pdf==TRUE){
		graphics.off()
	}
}





plotoutsens<- function(names,pdfname="no",mainname,var1,
	var2,sp=FALSE,pdf=TRUE){
	par <- extractModelParameters(names[[1]])
	parcheck <- fixdat(par)
	
	if(length(var2)==1){
		var2 <- rep(var2,length=length(names))
		
	}
	
	
	if(pdf==TRUE){
		pdf(pdfname,height=7,width=11)
	}
	# plotfun(parcheck,pdfname[[1]],"SP","TA")
	plotfun(parcheck,mainname[[1]],var1,var2[1],sp1=sp)
	
	sq <- seq(0,.5,length=length(names))
	colme <- seq(1,length(names))
	pchme <- seq(1,length(names))
	
	for(i in 2:length(names)){
		# print(i)

		partemp <- extractModelParameters(names[[i]])
		partemp <- fixdat(partemp)
		#		browser()
		pointsfun(partemp,var1,var2[i],eps=sq[i],
			cols=colme[i],pchs=pchme[i])
		
	}

	
	if(pdf==TRUE){
		graphics.off()
	}
}






#names,pdfname,mainname,legs,var1,
#	var2,var3,sp=FALSE,pdf=TRUE
#crosslag and autoregressive plots plots
plotsensARCL <- function(names1,pdfname,mains,
	n1,n2,legs,sp2=FALSE){
		
	if(length(n1)==1){
		n1 <- rep(n1,length=length(names))
		
	}
	
	pdf(pdfname)
	par(mfrow=c(2,2))
	#CL y x n1=tb, n2=sp
	plotoutsens(names=names1,mainname=mains[1],
		var1=n1[1],var2=n2,sp=sp2,pdf=FALSE)
	plotoutsens(names=names1, mainname =mains[2],
		var1=n2,var2=n1,sp=sp2,pdf=FALSE)
	#AR SPPB, TB
	print("AR")
	plotoutsens(names=names1, mainname =mains[3],
		var1=n2,var2=n2,sp=sp2,pdf=FALSE)
	plotoutsens(names=names1, mainname =mains[4],
		var1=n1[1],var2=n1[1],sp=sp2,pdf=FALSE)
	
	
	colme <- seq(1,length(names))
	pchme <- seq(1,length(names))
	legend("bottomright",legend=legs,pch=pchme,
		col=colme,lty=1)
	graphics.off()
	}
	
	
	
	
	
	
	
#AR ONLY PLOT FUN

#crosslag and autoregressive plots plots
plotsppbaro <- function(name,type="physical"){
	par <- extractModelParameters(name)
	parcheck <- fixdat(par)
	if(type=="physical"){
		plotfun(parcheck,"Autoregressive effects of SPPB","SP","SP")
	}else{
		
		plotfun(parcheck,"Autoregressive effects of Trails B","TB","TB")
		}
	}
	
	
plotwsaro <- function(name,type="physical"){
	par <- extractModelParameters(name)
	parcheck <- fixdat(par)
	

	if(type=="physical"){
		plotfun(parcheck,"Autoregressive effects of WS","WS","WS")
	}else{
		plotfun(parcheck,"Autoregressive effects of Trails B","TB","TB")
		}
	}
	
	
plottawsaro <- function(name,type="physical"){
		par <- extractModelParameters(name)
	parcheck <- fixdat(par)
	
	if(type=="physical"){
		plotfun(parcheck,"Cross Sectional Effects of Trails A on WS","WS","TA")
	}else{
		plotfun(parcheck,"Cross Sectional Effects of Trails A on Trails B","TB","TA")
	}
}



plottasparo <- function(name,type="physical"){
		par <- extractModelParameters(name)
	parcheck <- fixdat(par)
	
	if(type=="physical"){
		plotfun(parcheck,"Cross Sectional Effects of Trails A on SPPB","SP","TA")
	}else{
		plotfun(parcheck,"Cross Sectional Effects of Trails A on Trails B","TB","TA")
	}
}




	