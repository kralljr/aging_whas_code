######
# function to create tables in CLAR WHAS paper



#what are these	
	# #??

# nameMMSESPc <- file.path(hvl.dir, "sppb_mmse_censor_9apr13.out")
# nameMMSEWSc <- file.path(hvl.dir, "ws_mmse_censor_9apr13.out")	




#set working directories
age.dir <- "/Users/jennakrall/Dropbox/Aging/"
code.dir <- file.path(age.dir, "aging_code")
mod.dir <- file.path(age.dir, "final_models")



#source necessary code
source(file.path(code.dir, "parsing_mplus_files.R"))
source(file.path(code.dir, "aging_table_fn.R"))




#load standard deviations
load(file.path(age.dir, "datasets/sds_8apr13.RData"))



#unconstrained models
# add these
nameTBSP <- file.path(mod.dir, "unconstrain/sppb_tmtb_uncon.out")
nameTBWS <- file.path(mod.dir, "unconstrain/ws_tmtb_uncon.out")
nameHVLRSP <- file.path(mod.dir, "unconstrain/sppb_hvlr_uncon.out")
nameHVLRWS <- file.path(mod.dir, "unconstrain/ws_hvlr_uncon.out")
nameSQHVLDELSP <- file.path(mod.dir, "unconstrain/sppb_sqhvldel_uncon.out")
nameSQHVLDELWS <- file.path(mod.dir, "unconstrain/ws_sqhvldel_uncon.out")
nameMMSESP <- file.path(mod.dir, "unconstrain/sppb_mmse_uncon.out")
nameMMSEWS <- file.path(mod.dir, "unconstrain/ws_mmse_uncon.out")


#constrained models
CnameTBSP <- file.path(mod.dir, "sppb_tmtb.out")
CnameTBWS <- file.path(mod.dir, "ws_tmtb.out")
CnameHVLRSP <- file.path(mod.dir, "sppb_hvlr.out")
CnameHVLRWS <- file.path(mod.dir, "ws_hvlr.out")
CnameSQHVLDELSP <- file.path(mod.dir, "sppb_sqhvldel.out")
CnameSQHVLDELWS <- file.path(mod.dir, "ws_sqhvldel.out")
CnameMMSESP <- file.path(hvl.dir, "sppb_mmse.out")
CnameMMSEWS <- file.path(hvl.dir, "ws_mmse.out")







	
	
#phys to cog	
getests(CnameTBSP, "SP", "TB")	
getests(CnameTBWS, "WS", "TB")	
getests(CnameHVLRSP, "SP", "HV")	
getests(CnameHVLRWS, "WS", "HV")	
getests(CnameSQHVLDELSP, "SP", "HV")	
getests(CnameSQHVLDELWS, "WS", "HV")	
getests(CnameMMSESP, "SP", "MM")
getests(CnameMMSEWS, "WS", "MM")


#cog to phys
getests(CnameTBSP, "TB", "SP")	
getests(CnameTBWS, "TB", "WS")	
getests(CnameHVLRSP, "HV", "SP")	
getests(CnameHVLRWS, "HV", "WS")	
getests(CnameSQHVLDELSP, "HV", "SP")	
getests(CnameSQHVLDELWS, "HV", "WS")	
getests(CnameMMSEWS,  "MM", "WS")
getests(CnameMMSESP, "MM", "SP")














####################
#create pred phys table

listnames1 <- list(nameTBWS, nameTBSP, nameHVLRWS, nameHVLRSP, 
	nameSQHVLDELWS, nameSQHVLDELSP)
# rats <- rep(sds[c("TB", "HVLr", "SQHVLdel")], 2)/rep(sds[c("WS", "SP")], each = 3)r
# outPHYS <- tabfun(listnames1, "TB", "HV","HV", "WS", "SP", rats)
rats2 <- rep(1, 6)
outPHYS <- tabfunP(listnames1, "TB", "HV","HV", "WS", "SP", rats2)





####################
#create pred cog table

listnames1 <- list(nameTBWS, nameHVLRWS, nameTBSP, nameHVLRSP,
	nameSQHVLDELWS, nameSQHVLDELSP)
# rats <- rep(sds[c("WS", "SP")], each = 3)/rep(sds[c("TB", "HVLr", "SQHVLdel")], 2)	

outCOG <- tabfunC(listnames1, "WS", "SP", "TB", "HV", "HV" , rats2)






#####
# Format output
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




#get AR/CL
#cl
xtable(outPHYS2[1:6,])
#ar
xtable(outPHYS2[7:12,])
#cl
xtable(outCOG2[1:6,])
#ar
xtable(outCOG2[7:12,])
# write.csv(outCOG2, file = "outCOG_9apr13.csv")
# write.csv(outPHYS2, file = "outPHYS_9apr13.csv")













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

