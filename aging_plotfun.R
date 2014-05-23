	
getpars <- function(name1, name2, typey, typex, typey2, typex2,
	rats){
		
		
	par <- extractModelParameters(name1)
	parcheck <- fixdat(par,std=FALSE)
	par <- extractModelParameters(name2)
	parcheck2 <- fixdat(par,std=FALSE)
	
	ests <- regs(parcheck,typey,typex)
	lb <- (ests$est-1.96*ests$se) * rats[1]
	ub <- (ests$est+1.96*ests$se) * rats[1]
	ests <- ests$est * rats[1]
	
	ests2 <- regs(parcheck2,typey2,typex2)
	lb2 <- (ests2$est-1.96*ests2$se) * rats[2]
	ub2 <- (ests2$est+1.96*ests2$se) *rats[2]
	ests2 <- ests2$est * rats[2]
	
	
	ests2out <- cbind(ests2,lb2,ub2)
	if(length(ests2) == 4){
		
		ests2out <- rbind(ests2out[1:2,], rep(NA,3), ests2out[3:4,])
		
	}
	list(cbind(ests,lb,ub),ests2out)
}	



plotsidebyside <- function(name1, name2, typey, typex, typey2, typex2, ylims, xlims,
	mtext1, mtext2, stdx, stdy, col2 = colsall[2], pch2 = 17,
	seqs = seq(2,6), plot = NULL, plot2 = FALSE, ltys =1, lwds = 3,
	rats = c(1, 1), cexlab = 2.5, cexpt = 2, caxis = 3, axis1 = T, axis2 = F){
		
		
	getpars1 <- getpars(name1, name2, typey, typex, 
		typey2, typex2, rats = rats)
	
	dat1 <- getpars1[[1]]
	dat2 <- getpars1[[2]]
	# col1 <- "darkslategray4"
	# col2 <- "darkblue"

	
	# col1 <- colsall[1]
	
	# pch1 <- 16
	# pch2 <- 17
	if(typey == "WS") {
		seq1 <- seqs + .3
	}else{
		seq1 <- seqs + .1
		}

	if(plot2 == FALSE) {
	
	if(is.null(plot)) {
		plot(seqs, dat1[,1], xlim = xlims, ylim = ylims, col=col2, 
			pch=pch2, main = "", ylab = "", xlab = "",
			cex.lab = cexlab, cex = cexpt, cex.axis = caxis, axes = F)
			
			if(axis1 != F) {
				
				axis(1, at = seq1, labels = seqs,
					cex.axis = caxis, tick = T)
				mtext("Visit", side = 1, at = 3.5, cex = cexlab, line = 5)
				}
			 axis(2, cex.axis = caxis)
			 box()
		mtext(mtext1, cex= cexlab, line = 1)
		# mtext(paste("baseline stddev change in ",stdy," for a\n",
			# "baseline stddev increase in ",stdx, sep=""), side = 2, line = 3.5, cex = cexlab)
		
		# abline(h = 0, lty = 2, col = "grey50", lwd = lwds)
	}else{
		points(seqs, dat1[,1], pch = pch2, col = col2,
			cex = cexpt)
		}
	for( i in 1:5){
		segments(seqs[i], dat1[i,2], seqs[i], dat1[i,3], col=col2, lwd = lwds,lty = ltys)
		segments(seqs[i]-.1,dat1[i,2], seqs[i]+.1,dat1[i,2], col=col2, lwd = lwds)
		segments(seqs[i]-.1,dat1[i,3], seqs[i]+.1,dat1[i,3], col=col2, lwd = lwds)
	}
	}
	
	
	if(plot2 == TRUE) {
	#get rid of NAs
	dat2b <- data.frame(seqs, dat2)
	dat2b <- dat2b[complete.cases(dat2b), ]
	if(is.null(plot)) {
		plot(dat2b[,1], dat2b[,2], xlim = xlims, 
			ylim = ylims, col= col2, pch = pch2,
			yaxt = "n", main = "", ylab = "", xlab = "",
			cex.lab = cexlab, cex = cexpt, cex.axis = caxis, axes = F)
			#			 axis(2, cex.axis = caxis)
			if(axis1 != F) {
				axis(1, at = seq1, labels = seqs, cex.axis = caxis, tick = T)
				mtext("Visit", side = 1, at = 4.3, cex = cexlab, line = 3)
				}
			if(axis2 != F){
				axis(2, cex.axis = caxis, tick = T)
			}	
			 box()
		mtext(mtext2, cex = cexlab, line = 1)
		# abline(h = 0, lty = 2, col = "grey50", lwd=lwds)
	
	}else{
		points(dat2b[,1], dat2b[,2], pch = pch2, col = col2,
			cex = cexpt)
		}
	for( i in 1:5){
		segments(dat2b[i,1], dat2b[i,3], dat2b[i,1], 
			dat2b[i,4], col=col2, lwd = lwds, lty = ltys)
		segments(dat2b[i,1]-.1,dat2b[i,4], 
			dat2b[i,1]+.1,dat2b[i,4], col=col2, lwd = lwds)
		segments(dat2b[i,1]-.1,dat2b[i,3], 
			dat2b[i,1]+.1,dat2b[i,3], col=col2, lwd = lwds)
	}	
		}
	
}