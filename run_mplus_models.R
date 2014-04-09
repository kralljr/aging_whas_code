# R file of functions to create .inp Mplus files


library(MplusAutomation)

# setwd("/Users/jennakrall/Dropbox/Aging/final_models")

# cns <- c('imp1', 'imp2', 'baseid', 'ws1', 'ws2', 'ws3', 'ws4', 'ws5', 'ws6', 'trailb1', 'trailb2', 'trailb3', 'trailb4', 'trailb5', 'trailb6', 'traila1', 'traila2', 'traila3', 'traila4', 'traila5', 'traila6', 'hvlr1', 'hvlr2', 'hvlr3', 'hvlr4', 'hvlr5', 'hvlr6', 'hvldel1', 'hvldel2', 'hvldel3', 'hvldel4', 'hvldel5', 'hvldel6', 'age', 'health', 'race', 'ed', 'sppb1', 'sppb2', 'sppb3', 'sppb5', 'sppb6', 'disease', 'gds', 'gdsc', 'vision', 'death', 'rmvis', 'time2', 'time3', 'time4', 'time5', 'time6', 'mmse1', 'mmse2', 'mmse3', 'mmse4', 'mmse5', 'mmse6')

cns <- list(c('baseid', 'ws1-ws6'), c('tb1-tb6', 'ta1-ta6'), c('hvlr1-hvlr6', 'shvldel1-shvldel6'), c('age', 'health', 'race', 'ed'), c('sppb1', 'sppb2', 'sppb3', 'sppb5', 'sppb6'), c('disease', 'gds', 'gdsc'), c('vision', 'death', 'rmvis'), c('time2-time6', 'mmse1-mmse6'))






#################################
#### FUNCTION TO CREATE .inp FILE and run model in Mplus
# homedir is directory to run model
# outfile1 is Mplus input file e.g. test.inp
# physall is vector of phys function "ws" or "sppb"
# cogall is vector of cog function "hvlr", "trailb", "shvldel", "mmse"
# data file is path to data (e.g. "data/all_data.dat")
# colnames is all variables, in order
# centervar is list of variables ot center
# missvar is list of variables with missingness
# missval is value of missing
# constraint is one of "arp", "arc", "clp", "clc"
# test is one of "arp", "arc", "clp", "clc"
outMod <- function(homedir, outfile1, datafiles, 
	physall = NULL, cogall = NULL, 
	colnames = cns, 
	centervar = NULL, 
	missvar = NULL, 
	missval = 999, constraint = NULL, test = NULL,
	censor = NULL, resid = F, lags = 1) {
	
	if(is.null(physall) | is.null(cogall)) {
		physall <- c("ws", "sppb")
		cogall <- c("tb", "mmse", "hvlr", "shvldel")
	}
	

	#for all phys
	for(i in 1 : length(physall)) {
		#for all cog
		for(j in 1 : length(cogall)) {
			print(cogall[j])
			#specify datafile
			if(cogall[j] == "tb") {
				datafile1 <- datafiles[1]
			}else{
				datafile1 <- datafiles[2]
			}
			
			#specify outfile
			outfile <- paste0(physall[i], cogall[j], outfile1)
			print(outfile)
			outfile <- file.path(homedir, outfile)
			
			#create input file
			createMod(outfile, physall[i], cogall[j], datafile1, 
				colnames, centervar, missvar, 
				missval, constraint, test, censor, resid,
				lags)
		}
	}
	
	
	
		
	#run Mplus models
	runModels(homedir, showOutput = T, logFile = NULL)	

}




#################################
#### FUNCTION TO CREATE .inp FILE
# outfile is Mplus input file e.g. test.inp
# physv is either "ws" or "sppb"
# cogv is either "hvlr", "trailb", "shvldel", "mmse"
# data file is path to data (e.g. "data/all_data.dat")
# colnames is all variables, in order
# centervar is list of variables ot center
# missvar is list of variables with missingness
# missval is value of missing
# constraint is one of "arp", "arc", "clp", "clc"
# test is one of "arp", "arc", "clp", "clc"
createMod <- function(outfile, physv, cogv, datafile, 
	colnames = cns, 
	centervar = NULL, 
	missvar = NULL, 
	missval = 999, constraint = NULL, test = NULL, censor = NULL,
	resid = F, lags = 1) {
	
	
	#get sequence information
	seqs <- seq(1, 6)
	if(physv == "sppb") {seqs <- c(1, 2, 3, 5, 6)}
	
	#specify title
	title <- paste0("CLAR ", physv, " ", cogv, ", con ")
		
		
	#get centering variables	
	if(is.null(centervar)) {
		centervar <- list(c("age", "ed", "gds"), c("disease")) 
			#"disease")
		centervar[[3]] <- paste0("time", seqs[-1])
		
		if(cogv == "tb") {
			centervar[[2]] <- c(centervar[[2]], paste0("ta", seqs))
		}
	}
	
	
	#get missing variables
	if(is.null(missvar)) {
		missvar <- list(c("ws1-ws6", "tb1-tb6", 
			"ta1-ta6"),
			c("hvlr1-hvlr6", "shvldel1-shvldel6"), 
			paste0("sppb", c(1, 2, 3, 5, 6)), c("gds", "gdsc", 
			"death", "mmse1-mmse6"))
	}
	
	
	#get variables to use
	usenames <- list(c(paste0(physv, seqs)), 
		c(paste0(cogv, seqs)),
		c(paste0("time", seqs[-1])),
		c( "race", "ed", "disease"),
		# c( "race", "ed"), 
		c("age", "gds", "vision"))	
		# c("age", "gds"))
		# c("age"))
	if(cogv == "tb") {
		ta <- paste0("ta", seqs)
		usenames[[6]] <- ta
		
		#fix variables
		colnames[[1]] <- c(paste0("imp", seq(1, 2)), colnames[[1]])
	}
	
	
	#concatenate beginning info
	catinfo(cogv, title, outfile, datafile, colnames, 
		usenames, centervar, missvar, missval, censor)

	#get model info	
	getARCL(physv, cogv, outfile, seqs, censor, resid, lags)
	
	
	#test
	if(!is.null(test)) {
		testARCL(test, seqs, outfile)
	}
	
	#constraints
	if(!is.null(constraint)) {
		constrainARCL(constraint, seqs, outfile, lags)
	}
	
	#output
	cat("\nOUTPUT: \n\t STDYX;", file = outfile, 
		sep = "\n", append = T)
}







#################################
#### FUNCTION TO ADD CONSTRAINTS
# constraint is one of "arp", "arc", "clp", "clc"
# seqs is either seq(1, 6) or c(1, 2, 3, 5, 6)
# outfile is outfile name
constrainARCL <- function(constraint, seqs, outfile, lags = 1) {
	
	#model constraint row
	cat("\n", "MODEL CONSTRAINT:", file = outfile, 
		sep = "", append = T)
		
	
	#for each constraints
	n <- length(constraint)
	if(constraint[n] == "cl") {
		n <- n - 1
		ttk <- paste0(constraint[1], 2)
		ttk1 <- paste0(constraint[2], 2)
		cat("\n\t NEW(c*0);", file = outfile, 
				sep = "", append = T)		
		cat("\n\t", paste0(ttk, "=", ttk1, " + c;"), file = outfile, 
				sep = "", append = T)		
		# cat("\n\t", paste0(ttk, "=", ttk1, ";"), file = outfile, 
				# sep = "", append = T)
	}
	
	for(j in 1 : n) {
		#create constraints for each pair	
		for(i in 2 : (length(seqs) - 1)) {
			ttk <- paste0(constraint[j], seqs[i])
			ttk1 <- paste0(constraint[j], seqs[i + 1]	)
			cat("\n\t", paste0(ttk, "=", ttk1, ";"), file = outfile, 
				sep = "", append = T)
		}
	}

	if(lags != 1) {
		for(j in 1 : n) {
			#create constraints for each pair	
			for(i in 3 : (length(seqs) - 1)) {
				ttk <- paste0(constraint[j], "2", seqs[i])
				ttk1 <- paste0(constraint[j], "2", seqs[i + 1]	)
				cat("\n\t", paste0(ttk, "=", ttk1, ";"), file = outfile, 
					sep = "", append = T)
		}
	}
	}
}






#################################
#### FUNCTION TO START OUTFILE
# title is title
# outfile is output file
# data file is path for data
# colnames is variable names
# usenames is variables in analysis
# centervar is variables to center
# missvar is variables with missing values
catinfo <- function(cogv, title, outfile, datafile, cns, 
	usenames, centervar, missvar, missval = 999, censor = NULL) {
		
	#specify title
	cat("TITLE:", title, file = outfile, 
		sep = "\t", append = F)
		
	#specify data	
	cat("\nDATA: \t FILE IS", datafile, ";", file = outfile, 
		sep = " ", append = T)



	#if TMT-B, then imputation
	if(cogv == "tb") {
		cat("\n\tTYPE= IMPUTATION;", file = outfile, 
			sep = " ", append = T)
	}
	
	
	#specify variable names, use variables
	cat("\nVARIABLE: \t NAMES ARE ", paste(cns[[1]], collapse = " "), 
		file = outfile, 
		sep = " ", append = T)
	for(i in 2 : (length(cns) - 1)) {
		cat("\n\t", cns[[i]], file = outfile, 
			sep = " ", append = T)
	}	
	cat("\n\t", cns[[length(cns)]], ";", file = outfile, 
		sep = " ", append = T)
		
	
	
	cat("\n\tUSEVARIABLES ARE ", paste(usenames[[1]], collapse = " "), 
		file = outfile, 
		sep = " ", append = T)
	for(i in 2 : (length(usenames) - 1)) {
		cat("\n\t", usenames[[i]], file = outfile, 
		sep = " ", append = T)
	}	
	cat("\n\t", usenames[[length(usenames)]], ";", file = outfile, 
		sep = " ", append = T)
		
		
		
	
	#specify centering
	cat("\n\tCENTERING = GRANDMEAN (", paste(centervar[[1]], 
		collapse = " "),  file = outfile, 
		sep = " ", append = T)
	cat("\n\t", paste(centervar[[2]], collapse = " "), 
		file = outfile, 
		sep = " ", append = T)	
	cat("\n\t", paste(centervar[[3]], collapse = " "), ")", ";",
		file = outfile, 
		sep = " ", append = T)
		
		
		
	#specify missing
	cat("\n\tMISSING IS ", paste(missvar[[1]], collapse = " "), 
		file = outfile, 
		sep = " ", append = T)
	for(i in 2 : (length(missvar) - 1)) {
		cat("\n\t", missvar[[i]], file = outfile, 
		sep = " ", append = T)
	}	
	cat("\n\t", missvar[[length(missvar)]], 
		paste0("(", missval, ")"), ";", file = outfile, 
		sep = " ", append = T)


	if(!is.null(censor)) {
		cat("\n\tCENSORED ARE ", paste(censor, collapse = " "), 
			file = outfile, 
			sep = " ", append = T)
	}
	
	l1 <-  "\nANALYSIS: \t ESTIMATOR = mlr;"
	l2 <- "MODEL:"
	
	
	if(!is.null(censor)) {
		l1b <- "\nINTEGRATION = MONTECARLO;"
		cat(c(l1, l1b, l2), file = outfile, 
			sep = "\n", append = T)
	}else{
		cat(c(l1, l2), file = outfile, 
			sep = "\n", append = T)

		
		}
	

	
		
}








#################################
#### FUNCTION TO ADD MODEL TEST
# testtype is vector of "arp", "arc", "clp", "clc"
# seqs is either seq(1, 6) or c(1, 2, 3, 5, 6)
# outfile is outfile name
testARCL <- function(testtype, seqs, outfile) {
	
	#create model test row
	cat("MODEL TEST:", file = outfile, 
		sep = "", append = T)
		
	#for each constraint
	for(j in 1 : length(testtype)) {
		#test for each pair (global)
		for(i in 2 : (length(seqs) - 1)) {
			ttk <- paste0(testtype[j], seqs[i])
			ttk1 <- paste0(testtype[j], seqs[i + 1]	)
			cat("\n\t", paste0(ttk, "=", ttk1, ";"), file = outfile, 
				sep = "", append = T)
		}
	}
	
}






#################################
#### FUNCTION TO ADD CL/AR effects
#physvar is either "ws" or "sppb"
#cogv is either "hvlr", "trailb", "shvldel", "mmse"
#outfile is outfile name
# seqs is either seq(1, 6) or c(1, 2, 3, 5, 6)
getARCL <- function(physvar, cogv, outfile, seqs, 
	censor = NULL, resid = F, lags = 1) {
		
		
	for(k in 1 : length(seqs)) {
		
		
		#set current
		i <- seqs[k]
		
		#get Cog/Phys
		phys <- paste0(physvar, i)
		cog <- paste0(cogv, i)
		
		#get covariate effects
		p1 <- paste("\t", phys, "on",  "ed", "age", "race", 
			"vision", "gds", "disease")
		c1 <- paste(cog, "on",  "ed", "age", "race", 
			"vision", "gds", "disease")
			
			
		#add in TMT-A for TMT-B	
		if(cogv == "tb") {
			c1 <- paste(c1, paste0("ta", i))
			p1 <- paste(p1, paste0("ta", i))
			
		}
		
		#Add correlation at each time
		if(is.null(censor)) {
			cor1 <- paste0(phys, " with ", cog, ";")
		}else{
			cor1 <- ""
			}
		
		#Get CL/AR effects 
		if(i != 1) {
			
			#effects of time
			time1 <- paste0("time", i)
			p1 <- paste(p1, time1)
			c1 <- paste(c1, time1)

			#get previous cog/phys
			pphys <- paste0(physvar, seqs[k - 1])
			pcog <- paste0(cogv, seqs[k - 1])
			
			#specify CL/AR effects
			clp <- paste0(phys, " on ", pcog, 
				" (", "clp", i, ");")
			clc <- paste0(cog, " on ", pphys, 
				" (", "clc", i, ");")
			arp <- paste0(phys, " on ", pphys, 
				" (", "arp", i, ");")
			arc <- paste0(cog, " on ", pcog, 
				" (", "arc", i, ");")
					
				
			#add to file
			cat(c(clp, clc, arp, arc), 
				file = outfile, 
				sep = "\n\t", append = T)
				
			if(lags != 1 & k > 2) {
				pphys2 <- paste0(physvar, seqs[k - 2])
				pcog2 <- paste0(cogv, seqs[k - 2])
				
							#specify CL/AR effects
				clp2 <- paste0(phys, " on ", pcog2, 
					" (", "clp2", i, ");")
				clc2 <- paste0(cog, " on ", pphys2, 
					" (", "clc2", i, ");")
				arp2 <- paste0(phys, " on ", pphys2, 
					" (", "arp2", i, ");")
				arc2 <- paste0(cog, " on ", pcog2, 
					" (", "arc2", i, ");")
					
				#add to file
				cat(c(clp2, clc2, arp2, arc2), 
					file = outfile, 
					sep = "\n\t", append = T)	
					
				
			}	
				
			if(resid == T)	{
				corp <- paste(cog, "with", pcog, ";")
				cat("\n", corp, 
					file = outfile, 
					sep = "\n\t", append = T)
				corc <- paste(phys, "with", pphys, ";")
				cat("\n", corc, 
					file = outfile, 
					sep = "\n\t", append = T)				
					
			}
		}
		
		#end p1/c1
		p1 <- paste0(p1, ";")
		c1 <- paste0(c1, ";")
		
		
		#add to file
		cat(c(p1, c1, cor1), 
			file = outfile, 
			sep = "\n\t", append = T)
				
	
		}

}

