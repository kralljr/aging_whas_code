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



#phys to cog
phys <- c("WS", "SP")
cog <- c("TB", "HV", "SH", "MM")
k <- 1
for(i in 1 : length(phys)) {
	for(j in 1 : length(cog)) {
		getests(ageedrace[k], phys[i], cog[j])
		getests(ageedrace[k], cog[j], phys[i])
		k <- k + 1
	}
}

	














####################
#create pred phys table

rats2 <- rep(1, 8)
outPHYS <- tabfunP(ageedrace, "TB", "HV", "SH", "MM", "WS", "SP", rats2)
outPHYS <- tabfunP(con, "TB", "HV", "SH", "MM", "WS", "SP", rats2)




####################
#create pred cog table

outCOG <- tabfunC(ageedrace, "WS", "SP", "TB", "HV", "SH", "MM", rats2)






#####
# Format output
cogs <- c("TMT-B", "HVL-imm","HVL-del", "MMSE")
phys <- c("WS", "SPPB")

row2cg <- rep(phys, 6)
 row1cg <- rep(c(cogs[1], "", cogs[2], "", cogs[3], ""), 2)

row2ph <- rep(cogs, 4) 
row1ph <- rep(c(phys[1], "", "", "", phys[2], "", "", ""), 2)

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
###

#names
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











#####
	
	
#Get parameters for Tables 2 and 3 in main text

#outcome is cog or phys
nout <- c(4, 2)
nout.var <- list(c("TB", "HV", "SH", "MM"), c("WS", "SP"))
fp <- list(c("traila_trailb_time", "hvlr", "sqhvldel", "mmse"), c("ws", "sppb"))


sd1a <- list(sd1[c("trailb1", "hvlr1", "hvldel1", 
	"mmse1")], sd1[c("ws1", "sppb1")])


namef <- con

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
	
	
temp2 <- getests(file.path(hvl.dir, "sppb_mmse_constrain_17apr13.out"), "SP", "MM")	
temp <- as.character(temp2[[2]][1, 1])
substr(temp2[[2]][1, 1], 7, 10)

