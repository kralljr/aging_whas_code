#####
# File to standardize main data and imputed data
library(foreign)



olddat <- read.table("~/Dropbox/Aging/final_models/data/olddat/std_hvl_alllcovar_19jan13.dat", na.strings = "999")
colnames(olddat) <- c("baseid", "age", "health", "ed", "race", 
	paste0("ws", seq(1, 6)), paste0("trailb", seq(1, 6)), 
	paste0("traila", seq(1, 6)), paste0("sppb", c(1, 2, 3, 5, 6)),
	paste0("time", seq(2, 6)), paste0("hvlr", seq(1, 6)),
	paste0("hvldel", seq(1, 6)), paste0("sqhvldel", seq(1, 6)), 
	"disease", "gds", "gdsc", paste0("mmse", seq(1, 6)))

#standardize each outcome measure by baseline
dat.main <- "/Users/jennakrall/Dropbox/Aging/final_models/data/"
dat.dir <- c(file.path(dat.main, "imputed"), 
	file.path(dat.main, "imputed_all"))

data.all <- read.dta(file.path(dat.main, "all_aging_data.dta"))
cns <- c("impute1", "impute2", colnames(data.all))



#############################
#MULTIPLE IMPUTED DATASETS, TMT-B			

start.names <- c("ws", "trailb", "sppb", "hvlr",  "mmse", "hvldel")
names <- c("impute", "imputeendo")
mns <- array(dim = c(25, 6, 2))
sds <- mns
reps <- 1

#whether all impute, or 1 impute
for(l in 1 : 2) {
	reps <- 1
	#for each imputed dataset
	for(i in 1 : 5) {
		for(j in 1 : 5) {
		
			#read in data, replacing 999
			dat <- read.table(file.path(dat.dir[1], 
				paste0(names[1], i, j, ".dat")) , na.strings = 999)
			colnames(dat) <- cns
			
			
			
			#only use TMT-A from imputation with multiple outcomes
			if(l == 2) {
				dat2 <- read.table(file.path(dat.dir[2], 
					paste0(names[2], i, j, ".dat")) , na.strings = 999)
				colnames(dat2) <- cns
				dat2 <- dat2[, c("baseid", paste0("traila", seq(1, 6)))]
				dat <- dat[, -which(substr(colnames(dat), 1, 6) == "traila")]
				
				dat <- merge(dat, dat2, by = "baseid")	
			} 	
			#assign column names	
			
			
			#for each outcome, endogenous
			for(k in 1 : length(start.names)) {
				#get mean, sd from first
				name1 <- paste0(start.names[k], "1")
				print(c(name1, l, i, j))
				
				
				dat1 <- dat[, name1]
				if(k == 6) {
					dat1 <- dat1^2
				}
				mn <- mean(dat1, na.rm = T)
				sd <- sd(dat1, na.rm = T)
				
				mns[reps, k, l] <- mn
				sds[reps, k, l] <- sd
				
				#stdize each time	
				for(len in 1 : 6) {
					if(k != 3 | len != 4){
						namek <- paste0(start.names[k], len)
						
						dat1 <- dat[, namek]
						if(k == 6) {
							dat1 <- dat1^2
						}
						
						dat[, namek] <- (dat1 - mn) / sd
						
						
						namek2 <- namek
						if(k == 6) {
							namek2 <- paste0("sqhvldel", len)
						}
						print(all.equal(dat[, which(colnames(dat) == namek)], 
							olddat[, which(colnames(olddat) == namek2)]))
					}
				}		
			}
			
			#check with old
			
			
			
			#save data
			nameout <- paste0("std", names[l], i, j, ".dat")
			dat <- dat[, cns]
			
			print(nameout)
			
			 write.table(dat, file.path(dat.dir[l], nameout), col.names = F,
				 row.names = F, na = "999")
			reps <- reps + 1	
		}
	}
}

###############################


























###############################
# All dat


#standardize
#for each outcome, endogenous
dat <- data.all
mnsALL <- vector(, length  = length(start.names))
sdsALL <- mnsALL
for(k in 1 : length(start.names)) {
	#get mean, sd from first
	name1 <- paste0(start.names[k], "1")
	print(name1)

	d1 <- dat[, name1]
	if(k == 6) {
		d1 <- d1^2
	}
	
	
	mn <- mean(d1, na.rm = T)
	sd <- sd(d1, na.rm = T)
	
	mnsALL[k] <- mn
	sdsALL[k] <- sd
	
	#stdize each time	
	for(len in 1 : 6) {
		if(k != 3 | len != 4){
			namek <- paste0(start.names[k], len)
			d1 <- dat[, namek]
			
			if(k == 6) {
				d1 <- d1^2
			}
			namek <- paste0(start.names[k], len)
			dat[, namek] <- (d1 - mn) / sd
			
			
			namek2 <- namek
			if(k == 6) {
				namek2 <- paste0("sqhvldel", len)
			}
			print(all.equal(dat[, which(colnames(dat) == namek)], 
				olddat[, which(colnames(olddat) == namek2)]))
	
		}
	}		
}


mnssds <- rbind(mnsALL, sdsALL)
colnames(mnssds) <- start.names

#save data
nameout <- "std_all_aging_data.dat"
write.table(dat, file.path(dat.main, nameout), col.names = F,
	row.names = F, na = "999")








#create data file for imputation
names <- "1"
for(i in 1 : 5) {
	for(j in 1 : 5) {
		names <- c(names, paste0("stdimpute", i, j, ".dat"))
	}
}
cat(names[-1], sep = "\n", file = file.path(dat.main, "stdimpute.dat"))




#create data file for ENDO imputation
names <- "1"
for(i in 1 : 5) {
	for(j in 1 : 5) {
		names <- c(names, paste0("stdimputeendo", i, j, ".dat"))
	}
}
cat(names[-1], sep = "\n", file = file.path(dat.main, "stdimputeendo.dat"))



