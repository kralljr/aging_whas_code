

masterdir1 <- "C:/Users/Jenna Krall/Dropbox/Aging"
masterdir <-  file.path(masterdir1, "final_models")


source(file.path(masterdir1, "aging_code", "run_mplus_models.R"))

homedir <- file.path(masterdir, "runfiles")
outfile <- ".inp"
#datafile <-  
datafiles <- c("datuse/stdimpute.dat", "datuse/std_all_aging_data.dat")
physv <- "sppb"
cogv <- "trailb"

physv <- c("ws", "sppb")
cogv <- c("trailb", "hvlr", "shvldel", "mmse")


outMod(homedir, outfile, datafiles, physv, cogv)