#Paper results

dat.dir <- "/Users/jennakrall/Dropbox/Aging/final_models/data"

library(foreign)


dat <- read.dta(file.path(dat.dir, "all_aging_data.dta"))



getsumm <- function(coln) {
	if(coln != "sppb") {
		seqs <- seq(1, 6)
	}else{
		seqs <- c(1, 2, 3, 5, 6)
	}
	
	
	
	dat1 <- dat[, paste0(coln, seqs)]
	
	
	if(substr(coln, 1, 4) == "trai") {
		dat1 <- 24*60/dat1
	}
	
	out1 <- apply(dat1, 2, function(x) length(which(!is.na(x))))
	out2 <- round(apply(dat1, 2, mean, na.rm = T), 2)
	out3 <- round(apply(dat1, 2, sd, na.rm = T), 2)
	out4 <- round(apply(dat1, 2, min, na.rm = T), 2)
	out5 <- round(apply(dat1, 2, max, na.rm = T), 2)
	
	data.frame(out1, out2, out3, out4, out5)
}



meas <- c("ws", "sppb", "trailb", "hvlr", "hvldel", "mmse", "traila")
for(i in 1 : length(meas)) {
	print(getsumm(meas[i]))
}





gettab <- function(var) {
	print(table(dat[, var]))
	print(table(dat[, var])/ sum(table(dat[, var])))
	print(sum(table(dat[, var])))
}
meansd <- function(var) {
	print(c(mean(dat[, var], na.rm = T), sd(dat[, var], na.rm = T)))
}

meansd("age")
meansd("ed")
meansd("gds")

gettab("race")
gettab("disease")
gettab("vision")
