

getests <- function(name, typex, typey, rat = 1) {
	par <- extractModelParameters(name)
	parcheck <- fixdat(par, std = FALSE)
	
	typexs <- substr(parcheck$param, 1, 2)
	typeys <- substr(parcheck$var, 1, 2)
	onw <- parcheck$onw
	
	wh <- which(typexs == typex & 
		typeys == typey & onw == "ON")
		
	pars <- parcheck[wh, ]
	parmat <- matrix(nrow = nrow(pars), ncol = 3)
	parmat <- data.frame(parmat)
	# parmat[,2] <- paste(pars$param, pars$var)
	parmat[,1] <- round(pars$est, 2)
	
	#CONFIDENCE INTERVAL
	rnd <- 10
	lb <- round(pars$est - 2 * pars$se, 10) * rat
	ub <- round(pars$est + 2 * pars$se, 10) * rat
	out2 <- data.frame(paste(round(pars$est * rat, 10 ),
		" (", lb, ", ", ub, ")", sep = ""))
		
	whrow <- which(lb >= 0 | ub <= 0)
	if(length(whrow) >  0) {

		for(j in 1 : length(whrow)) {
			print(paste0("X=", typex, ", Y=", typey, 
				", ci=", out2[whrow[j], ]))
		}
	}	
	
	# #SE ONLY
	se <- round(pars$se * rat, 2)
	out <- data.frame(paste(round(pars$est * rat, 2),
		 " (", se, ")", sep = ""))
	
	list(rev(t(out)), out2)
}





incbreaks <- function(out1f) {
	blank <- ""
	c(out1f[1:2], blank, out1f[3:4])
}




makedfP <- function (name1, x1, y1, name2, x2, y2,
	name3, x3, y3, type = "no", rats = c(1, 1, 1)) {
		
		
	out1a <- getests(name1, x1, y1, rat = rats[1])
	out1b <-  getests(name2, x2, y2, rat = rats[2])
	out1c <- getests(name3, x3, y3, rat = rats[3])
	
	if(type == "SP") {
		out1 <- data.frame(rbind(incbreaks(out1a[[1]]), 
			incbreaks(out1b[[1]]), incbreaks(out1c[[1]])))
	}else{
		out1 <- data.frame(rbind(out1a[[1]], 
			out1b[[1]], out1c[[1]]))
		}
	rownames(out1) <- NULL
	
	list(out1, list(out1a[[2]], out1b[[2]], out1c[[2]]))
}


# listnames[[1]] is x1 y1
# listnames[[2]] is x1 y2
# listnames[[3]] is x2 y1
# listnames[[4]] is x2 y2
tabfunP <- function(listnames, x1, x2, x3, y1, y2, rats) {
	
	
	#ALL CL for WS
	#tb -> ws, hvlr -> ws, sqhvldel -> ws
	out1 <- makedfP(listnames[[1]], x1, y1, 
		listnames[[3]], x2, y1,
		listnames[[5]], x3, y1, rats = rats[1:3])
	#ALL CL for SPPB
	#tb -> sppb, hvlr -> sppb, sqhvldel -> sppb
	out2 <- makedfP(listnames[[2]], x1, y2, 
		listnames[[4]], x2, y2,
		listnames[[6]], x3, y2, type = "SP", rats = rats[4:6])
	#ALL AR for WS
	#tb -> ws, hvlr -> ws, sqhvldel -> ws
	out3 <- makedfP(listnames[[1]], y1, y1, 
		listnames[[3]], y1, y1,
		listnames[[5]], y1, y1)
	#ALL AR for SPPB
	#tb -> sppb, hvlr -> sppb, sqhvldel -> sppb
	out4 <- makedfP(listnames[[2]], y2, y2, 
		listnames[[4]], y2, y2,
		listnames[[6]], y2, y2, type = "SP")


	list(data.frame(rbind(out1[[1]], out2[[1]], out3[[1]], out4[[1]])),
		list(out1[[2]], out2[[2]], out3[[2]], out4[[2]]))
	
}







#####MAKE COGNITIVE TABLE

makedf <- function (name1, x1, y1, name2, x2, y2, type = "no", rats = c(1,1)) {
	out1a <- getests(name1, x1, y1, rat = rats[1])
	out1b2 <-  getests(name2, x2, y2, rat = rats[2])
	
	blank <- ""
	out1b <-  c(out1b2[[1]][1:2], blank, out1b2[[1]][3:4])
	

	out1 <-rbind(out1a[[1]], out1b)
		
	rownames(out1) <- NULL
	
	list(out1, list(out1a[[2]], out1b2[[2]]))
}


# listnames[[1]] is x1 y1
# listnames[[2]] is x1 y2
# listnames[[3]] is x2 y1
# listnames[[4]] is x2 y2
tabfunC <- function(listnames, x1, x2, y1, y2, y3, rats) {
	
	#all cl for tb
	#ws -> tb, sppb-> tb
	out1 <- makedfC(listnames[[1]], x1, y1, 
		listnames[[3]], x2, y1, rats = rats[1:2])
	#all cl for hvlimm
	#ws -> hvlr, sppb-> hvlr
	out2 <- makedfC(listnames[[2]], x1, y2, 
		listnames[[4]], x2, y2, rats = rats[3:4])	
	#all cl for hvldel
	#ws -> hvldel, sppb-> hvldel
	out3 <- makedfC(listnames[[5]], x1, y3, 
		listnames[[6]], x2, y3, rats = rats[5:6])	
		
	#allAR for tb	
	#ws -> tb, sppb-> tb
	out4 <- makedfC(listnames[[1]], y1, y1, 
		listnames[[3]], y1, y1)
	#all AR for hvlimm
	#ws -> hvlr, sppb-> hvlr
	out5 <- makedfC(listnames[[2]], y2, y2, 
		listnames[[4]], y2, y2)	
	#all AR for hvldel
	#ws -> hvldel, sppb-> hvldel
	out6 <- makedfC(listnames[[5]], y3, y3, 
		listnames[[6]], y3, y3)	


	list(data.frame(rbind(out1[[1]], out2[[1]], out3[[1]], 
		out4[[1]], out5[[1]], out6[[1]])),
		list(out1[[2]], out2[[2]], out3[[2]], 
		out4[[2]], out5[[2]], out6[[2]]))
	
}





getLRTpval <- function(name1, fp2, name2, type = "no") {
	par1 <- extractModelSummaries(name1)
	#par1 <- readModels(name1)
	name2 <- file.path(fp2, name2)
	par2 <- extractModelSummaries(name2)
	#par2 <- readModels(name2)
	#compareModels(par1, par2, show = "summaries")
	if(type == "impute") {
		llnull <- par2$LL_Mean
		llalt <- par1$LL_Mean
		c0 <- 1
		c1 <- 1
		
	}else{
		
		llnull <- par2$LL
		llalt <- par1$LL
		c0 <- par2$LLCorr
		c1 <- par1$LLCorr
	}
	# dfs <- par1$Param-par2$Param
	p1 <- par1$Param
	p0 <- par2$Param
	
	cd <- (p0 * c0 - p1 * c1)/(p0 - p1)
	
	teststat <- -2*(llnull - llalt) / cd
	
	round(1- pchisq(-2*llnull +2 * llalt, df = p1 - p0), 4)
}	

getall <- function(name1, fp2, pref2, suff2, cog, phys, typeimp = "no") {
	pvals <- vector(, length = 4)
	
	types <- paste(rep(c("CL", "AR"), each = 2), 
		rep(c(cog, phys), 2), sep = "")
		
	for	(i in 1 : 4) {
		name2 <- paste(pref2, types[i], suff2, sep = "")
		pvals[i] <- getLRTpval(name1, fp2, name2, typeimp)
	}
	names(pvals) <- types
	pvals
}

