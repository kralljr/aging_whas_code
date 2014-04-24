

masterdir1 <- "C:/Users/Jenna Krall/Dropbox/Aging"
masterdir <-  file.path(masterdir1, "final_models")


source(file.path(masterdir1, "aging_code", "run_mplus_models.R"))





################
# AJE resub: no constrain
homedir <- file.path(masterdir, "runfiles")
outfile <- "noconstrain_aje.inp"  
datafiles <- c("datuse/stdimpute.dat", "datuse/blank.dat")

physv <- c("ws", "sppb")
cogv <- c("tb", "hvlr", "shvldel", "mmse")


outMod(homedir, outfile, datafiles, physv, 
	cogv)





################
# AJE resub: constrain CL
homedir <- file.path(masterdir, "runfiles")
outfile <- "constrainCL_aje.inp"  
datafiles <- c("datuse/stdimpute.dat", "datuse/blank.dat")

physv <- c("ws", "sppb")
cogv <- c("tb", "hvlr", "shvldel", "mmse")


outMod(homedir, outfile, datafiles, physv, 
	cogv, constraint = c("clp", "clc"))






################
# AJE resub: constrain all
homedir <- file.path(masterdir, "runfiles")
outfile <- "constrain_all_aje.inp"  
datafiles <- c("datuse/stdimpute.dat", "datuse/blank.dat")

physv <- c("ws", "sppb")
cogv <- c("tb", "hvlr", "shvldel", "mmse")


outMod(homedir, outfile, datafiles, physv, 
	cogv, constraint = c("clp", "clc", "arp", "arc"))





################
# AJE resub: constrain all
homedir <- file.path(masterdir, "runfiles")
outfile <- "constrain_all_aje.inp"  
datafiles <- c("datuse/stdimpute.dat", "datuse/blank.dat")

physv <- c("ws", "sppb")
cogv <- c("tb", "hvlr", "shvldel", "mmse")


outMod(homedir, outfile, datafiles, physv, 
	cogv, constraint = c("clp", "clc", "arp", "arc"))






################
# AJE resub: constrain CLP
homedir <- file.path(masterdir, "runfiles")
outfile <- "constrain_clp_aje.inp"  
datafiles <- c("datuse/stdimpute.dat", "datuse/blank.dat")

physv <- c("ws", "sppb")
cogv <- c("tb", "hvlr", "shvldel", "mmse")


outMod(homedir, outfile, datafiles, physv, 
	cogv, constraint = c("clp"))





################
# AJE resub: constrain CLC
homedir <- file.path(masterdir, "runfiles")
outfile <- "constrain_clc_aje.inp"  
datafiles <- c("datuse/stdimpute.dat", "datuse/blank.dat")

physv <- c("ws", "sppb")
cogv <- c("tb", "hvlr", "shvldel", "mmse")


outMod(homedir, outfile, datafiles, physv, 
	cogv, constraint = c("clc"))



################
# AJE resub: constrain arC
homedir <- file.path(masterdir, "runfiles")
outfile <- "constrain_arc_aje.inp"  
datafiles <- c("datuse/stdimpute.dat", "datuse/blank.dat")

physv <- c("ws", "sppb")
cogv <- c("tb", "hvlr", "shvldel", "mmse")


outMod(homedir, outfile, datafiles, physv, 
	cogv, constraint = c("clc", "clp", "arc"))


################
# AJE resub: constrain arp
homedir <- file.path(masterdir, "runfiles")
outfile <- "constrain_arp_aje.inp"  
datafiles <- c("datuse/stdimpute.dat", "datuse/blank.dat")

physv <- c("ws", "sppb")
cogv <- c("tb", "hvlr", "shvldel", "mmse")


outMod(homedir, outfile, datafiles, physv, 
	cogv, constraint = c("clc", "clp", "arp"))




################
# AJE resub: constrain CL to be equal + c
homedir <- file.path(masterdir, "runfiles")
outfile <- "constrainCLplusc_aje.inp"  
datafiles <- c("datuse/stdimpute.dat", "datuse/blank.dat")

physv <- c("ws", "sppb")
cogv <- c("tb", "hvlr", "shvldel", "mmse")


outMod(homedir, outfile, datafiles, physv, 
	cogv, constraint = c("clp", "clc", "cl"))




################
# AJE resub: constrain CL to be all equal
homedir <- file.path(masterdir, "runfiles")
outfile <- "constrainCLallequal_aje.inp"  
datafiles <- c("datuse/stdimpute.dat", "datuse/blank.dat")

physv <- c("ws", "sppb")
cogv <- c("tb", "hvlr", "shvldel", "mmse")


outMod(homedir, outfile, datafiles, physv, 
	cogv, constraint = c("clp", "clc", "cl"))



































#####
# other models
#########
# change imputation
homedir <- file.path(masterdir, "runfiles")
outfile <- "impute_all.inp"  
datafiles <- c("datuse/stdimputeendo.dat", "datuse/std_all_aging_data.dat")


physv <- c("ws", "sppb")
cogv <- c("tb")


outMod(homedir, outfile, datafiles, physv, cogv, constraint = c("clp", "clc"))














########
# check censored
homedir <- file.path(masterdir, "runfiles")
outfile <- "censor_all.inp"  
datafiles <- c("datuse/stdimpute.dat", "datuse/std_all_aging_data.dat")


physv <- c("ws")
cogv <- c("tb")


outMod(homedir, outfile, datafiles, physv, cogv, constraint = c("clp", "clc"), 
	censor = "tb1-tb6(b);")

##
physv <- c("sppb")
cogv <- c("tb")


outMod(homedir, outfile, datafiles, physv, cogv, constraint = c("clp", "clc"), 
	censor = "tb1-tb3 tb5 tb6(b);")


##
physv <- c("sppb")
cogv <- c("mmse")


outMod(homedir, outfile, datafiles, physv, cogv, constraint = c("clp", "clc"), 
	censor = "mmse1-mmse3 mmse5 mmse6(a);")

##
physv <- c("ws")
cogv <- c("mmse")


outMod(homedir, outfile, datafiles, physv, cogv, constraint = c("clp", "clc"), 
	censor = "mmse1-mmse6(a);")


##
physv <- c("sppb")
cogv <- c("tb", "hvlr", "shvldel", "mmse")


outMod(homedir, outfile, datafiles, physv, cogv, constraint = c("clp", "clc"), 
	censor = "sppb1-sppb3 sppb5 sppb6(b);")













#########
# constrain all CL to be same, across phys/cog
homedir <- file.path(masterdir, "runfiles")
outfile <- "test_cl.inp"  
datafiles <- c("datuse/stdimpute.dat", "datuse/std_all_aging_data.dat")


physv <- c("ws", "sppb")
cogv <- c("tb", "hvlr", "shvldel", "mmse")



outMod(homedir, outfile, datafiles, physv, cogv, 
	constraint = c("clp", "clc", "cl"))






#########
#constrain all CL to be CL + c
homedir <- file.path(masterdir, "runfiles")
outfile <- "cl_plusc.inp"  
datafiles <- c("datuse/stdimpute.dat", "datuse/std_all_aging_data.dat")


physv <- c("ws", "sppb")
cogv <- c("tb", "hvlr", "shvldel", "mmse")



outMod(homedir, outfile, datafiles, physv, cogv, 
	constraint = c("clp", "clc", "cl"))







#########
# allow residual correlation
homedir <- file.path(masterdir, "runfiles")
outfile <- "resid_corr.inp"  
datafiles <- c("datuse/stdimpute.dat", "datuse/std_all_aging_data.dat")


physv <- c("ws", "sppb")
cogv <- c("tb", "hvlr", "shvldel", "mmse")


outMod(homedir, outfile, datafiles, physv, cogv, 
	constraint = c("clp", "clc"), resid = T)










#########
# add lags 2
homedir <- file.path(masterdir, "runfiles")
outfile <- "lags2_all.inp"  
datafiles <- c("datuse/stdimpute.dat", "datuse/std_all_aging_data.dat")


physv <- c("ws", "sppb")
cogv <- c("tb", "hvlr", "shvldel", "mmse")


#physv <- "ws"
#cogv <- "hvlr"

outMod(homedir, outfile, datafiles, physv, cogv, 
	constraint = c("clp", "clc"), lags = 2)

