######
# function to create tables in CLAR WHAS paper

tmt.dir <- "/Users/jennakrall/Dropbox/Aging/newimpute_4jul12/time2/"
hvl.dir <- "/Users/jennakrall/Dropbox/Aging/hvl_21aug12/"

load("/Users/jennakrall/Dropbox/Aging/datasets/sds_8apr13.RData")

source('~/Dropbox/Aging/rcode/parsing_mplus_files.R', chdir = F)
# sppb_traila_trailb_time_hvlr_21aug12.out
# sppb_traila_trailb_time_14jul12.out
load("/Users/jennakrall/Dropbox/Aging/datasets/sds_8apr13.RData")

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



nameTBSP <- file.path(tmt.dir, "sppb_traila_trailb_time_14jul12.out")
nameTBWS <- file.path(tmt.dir, "ws_traila_trailb_time_14jul12.out")
nameHVLRSP <- file.path(hvl.dir, "sppb_traila_trailb_time_hvlr_21aug12.out")
nameHVLRWS <- file.path(hvl.dir, "ws_traila_trailb_time_hvlr_21aug12.out")
nameSQHVLDELSP <- file.path(hvl.dir, 
	"sppb_traila_trailb_time_sqhvldel_8oct12.out")
nameSQHVLDELWS <- file.path(hvl.dir, 
	"ws_traila_trailb_time_sqhvldel_8oct12.out")


CnameTBSP <- file.path(tmt.dir, "sppb_traila_trailb_time_constrain_16apr13.out")
CnameTBWS <- file.path(tmt.dir, "ws_traila_trailb_time_constrain_16apr13.out")
CnameHVLRSP <- file.path(hvl.dir, "sppb_hvlr_constrain_16apr13.out")
CnameHVLRWS <- file.path(hvl.dir, "ws_hvlr_constrain_16apr13.out")
CnameSQHVLDELSP <- file.path(hvl.dir, 
	"sppb_sqhvldel_constrain_16apr13.out")
CnameSQHVLDELWS <- file.path(hvl.dir, 
	"ws_sqhvldel_constrain_16apr13.out")
	
	
#phys to cog	
getests(CnameTBSP, "SP", "TB")	
getests(CnameTBWS, "WS", "TB")	
getests(CnameHVLRSP, "SP", "HV")	
getests(CnameHVLRWS, "WS", "HV")	
getests(CnameSQHVLDELSP, "SP", "HV")	
getests(CnameSQHVLDELWS, "WS", "HV")	

#cog to phys
getests(CnameTBSP, "TB", "SP")	
getests(CnameTBWS, "TB", "WS")	
getests(CnameHVLRSP, "HV", "SP")	
getests(CnameHVLRWS, "HV", "WS")	
getests(CnameSQHVLDELSP, "HV", "SP")	
getests(CnameSQHVLDELWS, "HV", "WS")	


	
nameMMSESP <- file.path(hvl.dir, "sppb_mmse_9apr13.out")
nameMMSEWS <- file.path(hvl.dir, "ws_mmse_9apr13.out")
nameMMSESPc <- file.path(hvl.dir, "sppb_mmse_censor_9apr13.out")
nameMMSEWSc <- file.path(hvl.dir, "ws_mmse_censor_9apr13.out")	

CnameMMSESP <- file.path(hvl.dir, "sppb_mmse_constrain_17apr13.out")
CnameMMSEWS <- file.path(hvl.dir, "ws_mmse_constrain_17apr13.out")

getests(CnameMMSESP, "SP", "MM")
getests(CnameMMSEWS, "WS", "MM")
getests(CnameMMSEWS,  "MM", "WS")
getests(CnameMMSESP, "MM", "SP")

getests(nameTBSP, "SP", "TB")

####################
#create pred phys table
incbreaks <- function(out1f) {
	blank <- ""
	c(out1f[1:2], blank, out1f[3:4])
}

makedf <- function (name1, x1, y1, name2, x2, y2,
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
tabfun <- function(listnames, x1, x2, x3, y1, y2, rats) {
	
	
	#ALL CL for WS
	#tb -> ws, hvlr -> ws, sqhvldel -> ws
	out1 <- makedf(listnames[[1]], x1, y1, 
		listnames[[3]], x2, y1,
		listnames[[5]], x3, y1, rats = rats[1:3])
	#ALL CL for SPPB
	#tb -> sppb, hvlr -> sppb, sqhvldel -> sppb
	out2 <- makedf(listnames[[2]], x1, y2, 
		listnames[[4]], x2, y2,
		listnames[[6]], x3, y2, type = "SP", rats = rats[4:6])
	#ALL AR for WS
	#tb -> ws, hvlr -> ws, sqhvldel -> ws
	out3 <- makedf(listnames[[1]], y1, y1, 
		listnames[[3]], y1, y1,
		listnames[[5]], y1, y1)
	#ALL AR for SPPB
	#tb -> sppb, hvlr -> sppb, sqhvldel -> sppb
	out4 <- makedf(listnames[[2]], y2, y2, 
		listnames[[4]], y2, y2,
		listnames[[6]], y2, y2, type = "SP")


	list(data.frame(rbind(out1[[1]], out2[[1]], out3[[1]], out4[[1]])),
		list(out1[[2]], out2[[2]], out3[[2]], out4[[2]]))
	
}

listnames1 <- list(nameTBWS, nameTBSP, nameHVLRWS, nameHVLRSP, 
	nameSQHVLDELWS, nameSQHVLDELSP)
# rats <- rep(sds[c("TB", "HVLr", "SQHVLdel")], 2)/rep(sds[c("WS", "SP")], each = 3)r
# outPHYS <- tabfun(listnames1, "TB", "HV","HV", "WS", "SP", rats)
rats2 <- rep(1, 6)
outPHYS <- tabfun(listnames1, "TB", "HV","HV", "WS", "SP", rats2)









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
tabfun <- function(listnames, x1, x2, y1, y2, y3, rats) {
	
	#all cl for tb
	#ws -> tb, sppb-> tb
	out1 <- makedf(listnames[[1]], x1, y1, 
		listnames[[3]], x2, y1, rats = rats[1:2])
	#all cl for hvlimm
	#ws -> hvlr, sppb-> hvlr
	out2 <- makedf(listnames[[2]], x1, y2, 
		listnames[[4]], x2, y2, rats = rats[3:4])	
	#all cl for hvldel
	#ws -> hvldel, sppb-> hvldel
	out3 <- makedf(listnames[[5]], x1, y3, 
		listnames[[6]], x2, y3, rats = rats[5:6])	
		
	#allAR for tb	
	#ws -> tb, sppb-> tb
	out4 <- makedf(listnames[[1]], y1, y1, 
		listnames[[3]], y1, y1)
	#all AR for hvlimm
	#ws -> hvlr, sppb-> hvlr
	out5 <- makedf(listnames[[2]], y2, y2, 
		listnames[[4]], y2, y2)	
	#all AR for hvldel
	#ws -> hvldel, sppb-> hvldel
	out6 <- makedf(listnames[[5]], y3, y3, 
		listnames[[6]], y3, y3)	


	list(data.frame(rbind(out1[[1]], out2[[1]], out3[[1]], 
		out4[[1]], out5[[1]], out6[[1]])),
		list(out1[[2]], out2[[2]], out3[[2]], 
		out4[[2]], out5[[2]], out6[[2]]))
	
}

listnames1 <- list(nameTBWS, nameHVLRWS, nameTBSP, nameHVLRSP,
	nameSQHVLDELWS, nameSQHVLDELSP)
# rats <- rep(sds[c("WS", "SP")], each = 3)/rep(sds[c("TB", "HVLr", "SQHVLdel")], 2)	

outCOG <- tabfun(listnames1, "WS", "SP", "TB", "HV", "HV" , rats2)



cogs <- c("TMT-B", "HVL-imm","HVL-del")
phys <- c("WS", "SPPB")


row2cg <- rep(phys, 6)
 row1cg <- rep(c(cogs[1], "", cogs[2], "", cogs[3], ""), 2)

row2ph <- rep(cogs, 4) 
row1ph <- rep(c(phys[1], "", "", phys[2], "", ""), 2)

outCOG2 <- data.frame(row1cg, row2cg, outCOG[[1]])
outPHYS2 <- data.frame(row1ph,row2ph, outPHYS[[1]])

visits <- paste("visit", seq(2,6))
visits <- c("", "", visits)
colnames(outCOG2) <- visits
colnames(outPHYS2) <- visits

#cl
xtable(outPHYS2[1:6,])
#ar
xtable(outPHYS2[7:12,])
#cl
xtable(outCOG2[1:6,])
#ar
xtable(outCOG2[7:12,])
write.csv(outCOG2, file = "outCOG_9apr13.csv")
write.csv(outPHYS2, file = "outPHYS_9apr13.csv")













#######MMMSE
g1 <- getests(nameMMSEWS, "WS", "MM") #none sig
g2 <- getests(nameMMSEWS, "MM", "WS") #middle 3 are sig
g3 <- getests(nameMMSEWS, "WS", "WS") #all sig
g4 <- getests(nameMMSEWS, "MM", "MM") #all sig
preds <- c("WS", "MMSE", "WS", "MMSE")
outcome <- c("MMSE", "WS", "WS", "MMSE")
datout <- data.frame(outcome, preds, rbind(g1[[1]], g2[[1]], 
	g3[[1]], g4[[1]]))
xtable(datout)
	
g1 <- getests(nameMMSEWSc, "WS", "MM") #none sig
g2 <- getests(nameMMSEWSc, "MM", "WS") #middle 3 are sig
g3 <- getests(nameMMSEWSc, "WS", "WS") #all sig
g4 <- getests(nameMMSEWSc, "MM", "MM") #all sig
preds <- c("WS", "MMSE", "WS", "MMSE")
outcome <- c("MMSE", "WS", "WS", "MMSE")
datout
data.frame(outcome, preds, rbind(g1[[1]], g2[[1]], 
	g3[[1]], g4[[1]]))
	


g1 <- getests(nameMMSESP, "SP", "MM") #3rd sig
g2 <- getests(nameMMSESP, "MM", "SP") #none sig
g3 <- getests(nameMMSESP, "SP", "SP") #all sig
g4 <- getests(nameMMSESP, "MM", "MM") #all sig
preds <- c("SPPB", "MMSE", "SPPB", "MMSE")
outcome <- c("MMSE", "SPPB", "SPPB", "MMSE")
datout <- data.frame(outcome, preds, rbind(g1[[1]], g2[[1]], 
	g3[[1]], g4[[1]]))
xtable(datout)

g1 <- getests(nameMMSESPc, "SP", "MM") #3rd sig
g2 <- getests(nameMMSESPc, "MM", "SP") #none sig
g3 <- getests(nameMMSESPc, "SP", "SP") #all sig
g4 <- getests(nameMMSESPc, "MM", "MM") #all sig
preds <- c("SPPB", "MMSE", "SPPB", "MMSE")
outcome <- c("MMSE", "SPPB", "SPPB", "MMSE")
datout
data.frame(outcome, preds, rbind(g1[[1]], g2[[1]], 
	g3[[1]], g4[[1]]))
	
	
	
	
	
	
	
	
	
	
	
	





###
# p-vals for associations
names(outCOG[[2]]) <- paste0(rep(c("CL", "AR"), each =  3), 
	rep(c("TB", "HVLR", "SQHVLDEL"), 2))
for(i in 1 : 6) {
	names(outCOG[[2]][[i]]) <- c("WS", "SPPB")
}	
names(outPHYS[[2]]) <- paste0(rep(c("CL", "AR"), each =  2), 
	rep(c("WS", "SP"), 2))	
for(i in 1 : 4) {
	names(outPHYS[[2]][[i]]) <- c("TB", "HVLr", "SQHVLdel")
	
}	







######## 
#######
# GET PVALUES FOR LRT TESTS OF PARAMETER CHANGES ACROSS TIMES
######## 
#######


# # nameTBWS, nameHVLRWS, nameTBSP, nameHVLRSP,
	# nameSQHVLDELWS, nameSQHVLDELSP
	
	
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

# getLRTpval(nameTBWS, tmt.dir, "WS_tmtb_testtrend_ARTB_18dec12.out")

#HVL-imm and WS
hvlrws <- getall(nameHVLRWS, hvl.dir, "testtrend/WS_hvlr_testrend_",
	"_18dec12.out","HVL", "WS")
#HVL-imm and SP
hvlrsp <- getall(nameHVLRSP, hvl.dir, "testtrend/SPPB_hvlr_testtrend_",
	"_18dec12.out","HVL", "SP")
#HVL-del and WS
hvldelws <- getall(nameSQHVLDELWS, hvl.dir, "testtrend/WS_sqhvldel_testtrend_",
	"_18dec12.out","HVL", "WS")
#HVL-del and SP
hvldelsp <- getall(nameSQHVLDELSP, hvl.dir, "testtrend/SPPB_sqhvldel_testtrend_",
	"_18dec12.out","HVL", "SP")
	

	
#TMT-B and WS	
tmtbws <- getall(nameTBWS, tmt.dir, "WS_tmtb_testtrend_",
	"_18dec12.out","TB", "WS", typeimp = "impute")
#TMT-B and SP
tmtbsp <- getall(nameTBSP, tmt.dir, "sppb_tmtb_testtrend_",
	"_18dec12.out","TB", "SP", typeimp = "impute")
	
rbind(tmtbws, hvlrws, hvldelws)	
rbind(tmtbsp, hvlrsp, hvldelsp)	






##########MMSE
#MMSE and WS
mmsews <- getall(nameMMSEWS, hvl.dir, "WS_MMSE_testtrend_",
	"_9apr13.out","MMSE", "WS")
#MMSE and SP
mmsesp <- getall(nameMMSESP, hvl.dir, "SPPB_MMSE_testtrend_",
	"_9apr13.out","MMSE", "SP")		
mmsewsc <- getall(nameMMSEWS, hvl.dir, "WS_MMSE_censor_testtrend_",
	"_9apr13.out","MMSE", "WS")
#MMSE and SP
mmsespc <- getall(nameMMSESP, hvl.dir, "SPPB_MMSE_censor_testtrend_",
	"_9apr13.out","MMSE", "SP")	
mmsews
mmsewsc
mmsesp
mmsespc




######## GET LRT TESTS FOR PARTIAL CONSTRAIN
#MMSE/WS
getLRTpval(file.path(hvl.dir, "ws_mmse_constrain2_17apr13.out"), hvl.dir, 
	"ws_mmse_constrain_17apr13.out", type = "no") #not sig
#MMSE/SPPB
getLRTpval(file.path(hvl.dir, "sppb_mmse_constrain2_17apr13.out"), hvl.dir, 
	"sppb_mmse_constrain_17apr13.out", type = "no") #not sig	
	
#HVL-imm/WS
#not constrained, vs. constrain---- ALLOW ALL DIFFERENT
getLRTpval(file.path(hvl.dir, "ws_hvlr_constrain2_16apr13.out"), hvl.dir, 
	"ws_hvlr_constrain4_16apr13.out", type = "no") #not sig
getLRTpval(file.path(hvl.dir, "ws_traila_trailb_time_hvlr_21aug12.out"), hvl.dir, 
	"ws_hvlr_constrain3_16apr13.out", type = "no") #not sig

		
#HVL-imm/SPPB		
getLRTpval(file.path(hvl.dir, "sppb_hvlr_constrain2_16apr13.out"), hvl.dir, 
	"sppb_hvlr_constrain_16apr13.out", type = "no") #not sig
getLRTpval(file.path(hvl.dir, "sppb_hvlr_constrain3_16apr13.out"), hvl.dir, 
	"sppb_hvlr_constrain_16apr13.out", type = "no") #not sig
	
	
#HVL-del/WS
getLRTpval(file.path(hvl.dir, "ws_sqhvldel_constrain2_16apr13.out"), hvl.dir, 
	"ws_sqhvldel_constrain_16apr13.out", type = "no") #not sig
getLRTpval(file.path(hvl.dir, "ws_sqhvldel_constrain3_16apr13.out"), hvl.dir, 
	"ws_sqhvldel_constrain_16apr13.out", type = "no") #sig, p = 0.033, 2 = 3= 4
	
#HVL-del/SPPB
getLRTpval(file.path(hvl.dir, "sppb_sqhvldel_constrain2_16apr13.out"), hvl.dir, 
	"sppb_sqhvldel_constrain_16apr13.out", type = "no") #not sig
getLRTpval(file.path(hvl.dir, "sppb_sqhvldel_constrain3_16apr13.out"), hvl.dir, 
	"sppb_sqhvldel_constrain_16apr13.out", type = "no") #not sig
	
#TMT-b/WS
getLRTpval(file.path(tmt.dir, "ws_traila_trailb_time_constrain2_16apr13.out"), tmt.dir, 
	"ws_traila_trailb_time_constrain_16apr13.out", type = "impute") #not sig
getLRTpval(file.path(tmt.dir, "ws_traila_trailb_time_constrain3_16apr13.out"), tmt.dir, 
	"ws_traila_trailb_time_constrain_16apr13.out", type = "impute") #not sig
#TMT-B/SPPB	
getLRTpval(file.path(tmt.dir, "sppb_traila_trailb_time_constrain2_16apr13.out"), 
	tmt.dir, "sppb_traila_trailb_time_constrain_16apr13.out", 
	type = "impute") #sig p = 0.033
	
#ONLY SIG FOR 
#TMT-b -> SPPB 1/2 different from 3/4
#HVL-del ->ws 2=3=4 and 1=5
	




#####LRT FOR AR EFFECTS OF FINAL, Crosslagged Constrained models (compared to constraining AR effects)
#TB/WS
getLRTpval(file.path(tmt.dir, "ws_traila_trailb_time_constrain_16apr13.out"), tmt.dir, 
	"ws_traila_trailb_time_constrainARWS_16apr13.out", type = "impute") #sig p <0.001
getLRTpval(file.path(tmt.dir, "ws_traila_trailb_time_constrain_16apr13.out"), tmt.dir, 
	"ws_traila_trailb_time_constrainARTB_16apr13.out", type = "impute") #sig p =0.002
#TB/SP	
getLRTpval(file.path(tmt.dir, "sppb_traila_trailb_time_constrain_16apr13.out"), tmt.dir, 
	"sppb_traila_trailb_time_constrainARSP_16apr13.out", type = "impute") #not sig p =0.090
getLRTpval(file.path(tmt.dir, "sppb_traila_trailb_time_constrain_16apr13.out"), tmt.dir, 
	"sppb_traila_trailb_time_constrainARTB_16apr13.out", type = "impute") #sig p <0.001
	
	
#HVLimm/WS
getLRTpval(file.path(hvl.dir, "ws_hvlr_constrain_16apr13.out"), hvl.dir, 
	"ws_hvlr_constrainARWS_16apr13.out", type = "no") #not sig p <0.001
getLRTpval(file.path(hvl.dir, "ws_hvlr_constrain_16apr13.out"), hvl.dir, 
	"ws_hvlr_constrainARHV_16apr13.out", type = "no") #not sig p = 0.017
#HVLimm/SP
getLRTpval(file.path(hvl.dir, "sppb_hvlr_constrain_16apr13.out"), hvl.dir, 
	"sppb_hvlr_constrainARSP_16apr13.out", type = "no") #not sig p 0.070
getLRTpval(file.path(hvl.dir, "sppb_hvlr_constrain_16apr13.out"), hvl.dir, 
	"sppb_hvlr_constrainARHV_16apr13.out", type = "no") #not sig p = 0.029

#HVLdel/WS
getLRTpval(file.path(hvl.dir, "ws_sqhvldel_constrain_16apr13.out"), hvl.dir, 
	"ws_sqhvldel_constrainARWS_16apr13.out", type = "no") #not sig p <0.001
getLRTpval(file.path(hvl.dir, "ws_sqhvldel_constrain_16apr13.out"), hvl.dir, 
	"ws_sqhvldel_constrainARHV_16apr13.out", type = "no") #not sig p = 0.230
#HVLdel/SP		
getLRTpval(file.path(hvl.dir, "sppb_sqhvldel_constrain_16apr13.out"), hvl.dir, 
	"sppb_sqhvldel_constrainARSP_16apr13.out", type = "no") #not sig p = 0.076
getLRTpval(file.path(hvl.dir, "sppb_sqhvldel_constrain_16apr13.out"), hvl.dir, 
	"sppb_sqhvldel_constrainARHV_16apr13.out", type = "no") #not sig p = 0.258
	
#MMSE/WS
getLRTpval(file.path(hvl.dir, "ws_mmse_constrain_17apr13.out"), hvl.dir, 
	"ws_mmse_constrainARWS_17apr13.out", type = "no") #sig p< 0.001
getLRTpval(file.path(hvl.dir, "ws_mmse_constrain_17apr13.out"), hvl.dir, 
	"ws_mmse_constrainARHV_17apr13.out", type = "no") #sig p=0.042	
#MMSE/SPPB
getLRTpval(file.path(hvl.dir, "sppb_mmse_constrain_17apr13.out"), hvl.dir, 
	"sppb_mmse_constrainARSP_17apr13.out", type = "no") #not sig p=0.101	
getLRTpval(file.path(hvl.dir, "sppb_mmse_constrain_17apr13.out"), hvl.dir, 
	"sppb_mmse_constrainARMM_17apr13.out", type = "no") #sig	p = 0.014		
	
	
	
	
	
	
#Get parameters for Tables 2 and 3 in main text

#outcome is cog or phys
nout <- c(4, 2)
nout.var <- list(c("TB", "HV", "HV", "MM"), c("WS", "SP"))
fp <- list(c("traila_trailb_time", "hvlr", "sqhvldel", "mmse"), c("ws", "sppb"))


#check MMSE sd from stdize_data_9apr13.R
sds1 <- list(c(sds[c(2, 4, 5)], 1.731428), sds[c(1, 3)])

mat <- list()
for(i in 1 : 2) {
	mat[[i]] <- matrix(nrow = 8, ncol = 3)
	l <- 1
	for(k in 1 : nout[i]) {
		typey <- nout.var[[i]][k]
		fp1 <- fp[[i]][k]
		
		sd1 <- sds1[[i]][k]
		for(j in 1 : nout[3 - i]) {
				typex <- nout.var[[3 - i]][j]
				fp2 <- fp[[3 - i]][j]
				print(c(typey, typex))
				
				if(typey == "MM" | typex == "MM") {
					cons <- "_constrain_17apr13.out"
				}else {
					cons <- "_constrain_16apr13.out"
				}
				
				if(i == 2) {
					fps <- paste0(fp1, "_", fp2)
				}else{
					fps <- paste0(fp2, "_", fp1)
				}
				
				if(typey == "TB" | typex == "TB") {
					dir <- tmt.dir
				} else {
					dir <- hvl.dir
				}
				
				file.name <- paste0(fps, cons)
				print(file.name)
				temp1 <- getests(file.path(dir, file.name), 
					typex, typey)
				tempse <- as.numeric(substr(temp1[[1]][1], 7, 10))
				temp <- temp1[[2]][1, 1]
				temp <- as.character(temp)	
				
				est <- as.numeric(substr(temp, 1, 5))
				ci <- strsplit(strsplit(temp, "\\(")[[1]][2], ",")
				ci1 <- round(as.numeric(ci[[1]][1]) * sd1, 3)
				ci2 <- round(as.numeric(strsplit(ci[[1]][2], ")")[[1]][1]) * 
					sd1, 3)
				
				est <- est * sd1
				
				lb <- round(est - 1.96 * tempse * sd1, 3)
				ub <- round(est + 1.96 * tempse * sd1, 3)
				est <- round(est, 3)
				
				
				estci <- paste0(est, " (", lb, ", ", ub, ")")
				
				mat[[i]][l, 1] <- temp
				mat[[i]][l, 2] <- estci
				mat[[i]][l, 3] <- paste0(est, " (", ci1, ", ", ci2, ")")
				l <- l + 1
			} #end loop over x
		}#end loop over y
	
	}#end loop over table
	
	
temp2 <- getests(file.path(hvl.dir, "sppb_mmse_constrain_17apr13.out"), "SP", "MM")	
temp <- as.character(temp2[[2]][1, 1])
substr(temp2[[2]][1, 1], 7, 10)

