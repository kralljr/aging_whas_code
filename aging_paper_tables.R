######
# function to create tables in CLAR WHAS paper



#what are these	
	# #??

# nameMMSESPc <- file.path(hvl.dir, "sppb_mmse_censor_9apr13.out")
# nameMMSEWSc <- file.path(hvl.dir, "ws_mmse_censor_9apr13.out")	

library(xtable)


#set working directories
age.dir <- "/Users/jennakrall/Dropbox/Aging/"
code.dir <- file.path(age.dir, "aging_code")
mod.dir <- file.path(age.dir, "final_models")
outfile <- file.path(mod.dir, "runfiles")


#source necessary code
source(file.path(code.dir, "parsing_mplus_files.R"))
source(file.path(code.dir, "aging_table_fn.R"))




#load standard deviations
load(file.path(age.dir, "final_models/data/all_sds.RData"))



#get directory, file names
physv <- c("ws", "sppb")
cogv <- c("tb", "hvlr", "shvldel", "mmse")


ageedrace <- createDIR(file.path(outfile, "ageedrace"), "ageedrace")
disvisgds <- createDIR(file.path(outfile, "diseasegdsvision"), "")
uncon <- createDIR(file.path(outfile, "unconstrain"), "_uncon")
con <- createDIR(file.path(outfile, "constrain_ageedrace"), "ageedrace_c")
concl <- createDIR(file.path(outfile, "constrainCL_aje"), "constrainCL_aje")
nocon <- createDIR(file.path(outfile, "noconstrain_aje"), "noconstrain_aje")
conclp <- createDIR(file.path(outfile, "constrainclp_aje"), "constrain_clp_aje")
conclc <- createDIR(file.path(outfile, "constrainclc_aje"), "constrain_clc_aje")
conarp <- createDIR(file.path(outfile, "constrainarp_aje"), "constrain_arp_aje")
conarc <- createDIR(file.path(outfile, "constrainarc_aje"), "constrain_arc_aje")
conplusc <- createDIR(file.path(outfile, "constrainclplusc_aje"), "constrainclplusc_aje")
conclallequal <- createDIR(file.path(outfile, "constrainclallequal_aje"), "constrainclallequal_aje")



#phys to cog
phys <- c("WS", "SP")
cog <- c("TB", "HV", "SH", "MM")
k <- 1
for(i in 1 : length(phys)) {
	for(j in 1 : length(cog)) {
		getests(concl[k], phys[i], cog[j])
		getests(concl[k], cog[j], phys[i])
		k <- k + 1
	}
}

	












########
#  CONSTRAINT CL

####################
#create pred phys table

rats2 <- rep(1, 8)
outPHYS <- tabfunP(concl, "TB", "HV", "SH", "MM", "WS", "SP", rats2)

####################
#create pred cog table

outCOG <- tabfunC(concl, "WS", "SP", "TB", "HV", "SH", "MM", rats2)


#names
names(outCOG[[2]]) <- paste0(rep(c("CL", "AR"), each =  4), 
	rep(c("TB", "HVLR", "SQHVLDEL", "MMSE"), 2))
for(i in 1 : 6) {
	names(outCOG[[2]][[i]]) <- c("WS", "SPPB")
}	
names(outPHYS[[2]]) <- paste0(rep(c("CL", "AR"), each =  2), 
	rep(c("WS", "SP"), 2))	
for(i in 1 : 4) {
	names(outPHYS[[2]][[i]]) <- c("TB", "HVLr", "SQHVLdel", "MMSE")
	
}	








########
# NO CONSTRAINTS
####################
#create pred phys table

rats2 <- rep(1, 8)
outPHYSN <- tabfunP(nocon, "TB", "HV", "SH", "MM", "WS", "SP", rats2)

outPHYSpval <- tabfunP(nocon, "TB", "HV", "SH", "MM", "WS", "SP", rats2, ests = "pvals")
####################
#create pred cog table

outCOGN <- tabfunC(nocon, "WS", "SP", "TB", "HV", "SH", "MM", rats2)
outCOGpval <- tabfunC(nocon, "WS", "SP", "TB", "HV", "SH", "MM", rats2, ests = "pvals")




#####
# Format output
cogs <- c("TMT-B", "HVL-imm","HVL-del", "MMSE")
phys <- c("WS", "SPPB")

row2cg <- rep(phys, 8)
 row1cg <- rep(c(cogs[1], "", cogs[2], "", cogs[3], "", cogs[4], ""), 2)

row2ph <- rep(cogs, 4) 
row1ph <- rep(c(phys[1], "", "", "", phys[2], "", "", ""), 2)

outCOG2 <- data.frame(row1cg, row2cg, outCOGN[[1]])
outPHYS2 <- data.frame(row1ph,row2ph, outPHYSN[[1]])

visits <- paste("visit", seq(2,6))
visits <- c("", "", visits)
colnames(outCOG2) <- visits
colnames(outPHYS2) <- visits




#get AR/CL
#cl
xtable(outPHYS2[1:8,])
#ar
xtable(outPHYS2[9:16,])
#cl
xtable(outCOG2[1:8,])
#ar
xtable(outCOG2[7:12,])
# write.csv(outCOG2, file = "outCOG_9apr13.csv")
# write.csv(outPHYS2, file = "outPHYS_9apr13.csv")
###



###
# get LRT pvals for CL

mat <- matrix(nrow = length(nocon), ncol = 2)
for(i in 1 : length(nocon)) {
	mat[i, 1] <- getLRTpval(nocon[i], conclp[i], type = "impute")
	mat[i, 2] <- getLRTpval(nocon[i], conclc[i], type = "impute")
}
colnames(mat) <- c("phys", "cog")
rownames(mat) <- sapply(strsplit(nocon, "/"), function(x) x[10])


###
# get LRT pvals for AR

mat <- matrix(nrow = length(nocon), ncol = 2)
for(i in 1 : length(nocon)) {
	mat[i, 1] <- getLRTpval(concl[i], conarp[i], type = "impute")
	mat[i, 2] <- getLRTpval(concl[i], conarc[i], type = "impute")
}
colnames(mat) <- c("phys", "cog")
rownames(mat) <- sapply(strsplit(nocon, "/"), function(x) x[10])









#####
	
	
#Get parameters for Tables 2 and 3 in main text

#outcome is cog or phys
nout <- c(4, 2)
nout.var <- list(c("TB", "HV", "SH", "MM"), c("WS", "SP"))
fp <- list(c("traila_trailb_time", "hvlr", "sqhvldel", "mmse"), c("ws", "sppb"))


sd1a <- list(sd1[c("trailb1", "hvlr1", "hvldel1", 
	"mmse1")], sd1[c("ws1", "sppb1")])


namef <- concl

#for phys/cog
mat <- list()
for(i in 1 : 2) {
	mat[[i]] <- matrix(nrow = 8, ncol = 3)
	l <- 1
	
	#for each phys.or cog
	for(k in 1 : nout[i]) {
		
		#namey
		typey <- nout.var[[i]][k]

		
		sdU <- sd1a[[i]][k]
		
		#for each in other
		for(j in 1 : nout[3 - i]) {
			
				#namex
				typex <- nout.var[[3 - i]][j]
				

				print(c(typey, typex))
				if(i == 2) {
					fp1 <- namef[l]
				}else{
					seql <- c(1, 5, 2, 6, 3, 7, 4, 8)
					fp1 <- namef[seql[l]]
				}
				
				temp1 <- getests(fp1, typex, typey)
				
				
				tempse <- as.numeric(substr(temp1[[1]][1], 7, 10))
				temp <- temp1[[2]][1, 1]
				temp <- as.character(temp)	
				
				est <- as.numeric(substr(temp, 1, 5))
				ci <- strsplit(strsplit(temp, "\\(")[[1]][2], ",")
				ci1 <- round(as.numeric(ci[[1]][1]) * sdU, 3)
				ci2 <- round(as.numeric(strsplit(ci[[1]][2], ")")[[1]][1]) * 
					sdU, 3)
				
				est <- est * sdU
				
				#dont use
				lb <- round(est - 1.96 * tempse * sdU, 3)
				ub <- round(est + 1.96 * tempse * sdU, 3)
				est <- round(est, 3)
				
				
				estci <- paste0(est, " (", lb, ", ", ub, ")")
				
				mat[[i]][l, 1] <- temp
				mat[[i]][l, 2] <- estci
				mat[[i]][l, 3] <- paste0(est, " (", ci1, ", ", ci2, ")")
				l <- l + 1
			} #end loop over x
		}#end loop over y
	
	}#end loop over table
names(mat) <- c("cog", "phys")
	
temp2 <- getests(file.path(hvl.dir, "sppb_mmse_constrain_17apr13.out"), "SP", "MM")	
temp <- as.character(temp2[[2]][1, 1])
substr(temp2[[2]][1, 1], 7, 10)






#####
# get residual corr
for(i in 1 : length(concl)) {
	par1 <- extractModelParameters(concl[i])[[2]]
	name1 <- sapply(strsplit(par1[, 1], "\\."), function(x) x[2])
	par1 <- par1[which(name1 == "WITH"), ]
	if(i == 1) {
		pars <- par1
	}else{
		pars <- rbind(pars, par1)
		
	}
	
}




####
# get param info
for(i in 1 : length(conplusc)) {
	par1 <- extractModelParameters(conplusc[i])[[1]]
	name1 <- sapply(strsplit(par1[, 1], "\\."), function(x) x[1])
	par1 <- as.numeric(par1[which(name1 == "New"), ])
	est <- par1[3]
	ci <- est + c(-1.96, 1.96) * par1[4]
	par1 <- c(est, ci)
	if(i == 1) {
		pars <- par1
	}else{
		pars <- rbind(pars, par1)
		
	}
	
}
rownames(pars) <- sapply(strsplit(conplusc, "/"), function(x) x[10])

###
# get LRT pvals for CL

mat <- matrix(nrow = length(concl), ncol = 2)
for(i in 1 : length(concl)) {
	mat[i, 1] <- getLRTpval(concl[i], conclallequal[i], type = "impute")
}
colnames(mat) <- c("phys", "cog")
rownames(mat) <- sapply(strsplit(concl, "/"), function(x) x[10])
