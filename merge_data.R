#### File to clean, append all WHAS data

library(foreign)


dat.dir <- "/Users/jennakrall/Dropbox/Aging/final_models/data"
dat.dir <- "C:/Users/jrkrall/Dropbox/Aging/final_models/data"

olddat <- read.table(file.path(dat.dir, "olddat/hvl_alllcovar_19jan13.dat"), na.strings = "999")






#######################
####
# Physical/cog outcomes file
whas <- read.dta(file.path(dat.dir, "jenna.dta"))

#get 395
allNA <- rowSums(is.na(whas[, paste0("traila", seq(1, 6))]))
whas <- whas[-which(allNA == 6), ]

#get rid of 0 in ws
for(i in 1 : 6) {
	name <- paste0("r", i, "slow")
	wh0 <- which(whas[, name] == 0)
	print(length(wh0))
	if(length(wh0) > 0) {
		whas[wh0, name] <- NA
	}
}


#reverse HVL, HVL-del
for(i in 1 : 6) {
	name1 <- paste0("hvlr", i)
	name2 <- paste0("hvldel", i)
	whas[, name1] <- 36 - whas[, name1]
	whas[, name2] <- 12 - whas[, name2]
}


whas$scrptrc3 <- whas$scrptrc3 - 1
whas$scrptrc3[which(whas$scrptrc3 == 2)] <- 1






####
# SPPB
sppb <- read.dta(file.path(dat.dir,"sppb_r7.dta"))




















####
# Visit dates and Mortality
mort <- read.dta(file.path(dat.dir, "mstat7_w2.dta"))
whN <- which(substr(colnames(mort), 3, 6) == "date")

visdates <- matrix(nrow = nrow(mort), ncol = (length(whN) - 2))
j <- 1
temp <- as.Date(mort[, whN[1]], origin = "1960-01-01")
tempout <- temp
times <- c(1.5, 3, 6, 7.5, 9)
#don't include last visit
for(i in 2 : (length(whN) - 1)) {
	temp2 <- as.Date(mort[, whN[i]], origin = "1960-01-01")
	#save visit date
	tempout <- data.frame(tempout, temp2)
	
	diffs <- (temp2 - temp) / 365.25
	whNA <- which(is.na(diffs))
	if(length(whNA) > 0) {
		diffs[whNA] <- rep(times[j], length(whNA))
	}
	
	visdates[, j] <- diffs
	j <- j + 1
}

visdates <- data.frame(mort$baseid, visdates)
colnames(visdates) <- c("baseid", paste0("time", seq(2, 6)))



#get death dates
death <- mort[, c("baseid", "dated")]
death[, 2] <- as.Date(death[, 2], origin = "1960-01-01")


# check death and visit dates
whD <- which(!is.na(death[, 2] ))
rmvis <- vector(, length = nrow(death))
rmvis2 <- rmvis
for(i in 1: length(whD)) {
	dates <- death[whD[i], 2] - as.Date(as.numeric(tempout[whD[i], ]), origin = "1970-01-01")
	whM <- which(dates < 180 & dates > 0 & !is.na(dates))
	whM2 <- which(dates < 180 & !is.na(dates) & dates < 0)
	if(length(whM) > 0) {
		if(length(whM) > 1) {browser()}
		print(c( tempout[whD[i], whM[1]], death[whD[i], 2]))
		rmvis[whD[i]] <- whM[1]	
	}
	
	if(length(whM2) > 0 ) {
		# browser()
		# rmvis[whD[i]] <- whM2[1]
		rmvis2[whD[i]] <- 999
	}
}

death <- data.frame(death, rmvis, rmvis2)












####
# Dementia











####
# Visual impairment
vi <- read.dta(file.path(dat.dir, "scranl1b.dta"))
whN <- which(substr(colnames(vi), 4, 6) == "vsn" | 
	colnames(vi) %in% c( "baseid"))
vi <- vi[, whN]










####
# File with chronic disease, GDS
datphys <- read.dta(file.path(dat.dir, "w2phyact_final.dta"))

#restrict to chronic disease, GDS
whc <- which(colnames(datphys)%in%c("disease","gds","gdsc"))
datphys <- datphys[, c(1, whc)]


















####
# merge all data


#####
# Merge outcome measures
merged <- merge(whas, sppb, by = "baseid", all.x = T, all.y = F)

# Merge with disease, GDS, GDSc
merged <- merge(merged, datphys, by = "baseid", all.x = T, all.y = F)

# Merge with VI data
merged <- merge(merged, vi, by = "baseid", all.x = T, all.y = F)

# Merge with death date
merged <- merge(merged, death, by = "baseid", all.x = T, all.y = F)

#replace time with visdates
merged <- merge(merged, visdates, by = "baseid", all.x = T, all.y = F)
timeO <- which((substr(colnames(merged), 1, 4) == "time" & 
	sapply(strsplit(colnames(merged), "\\."), function(x) x[2])  == "x") | 
	colnames(merged) == "time1")
merged <- merged[, -timeO]	
timeN <- which(substr(colnames(merged), 1, 4) == "time")	
colnames(merged)[timeN] <- paste0("time", seq(2, 6))




#reduce to columns of interest
cn <- which(colnames(merged) %in% c("baseid", "scrgenhl", "scrptrc3", 
	"scrpted2", "disease", "gds", "gdsc", "scrvsnpb", "dated", "rmvis",
	"scrptage") |
	substr(colnames(merged), 1, 3) %in% c("tra", "hvl", "tim") |
	substr(colnames(merged), 3, 5) %in% c("phy", "slo"))
cn2 <- which(substr(colnames(merged), 2, 2) == "7")
cn3 <- which(substr(colnames(merged), 1, 6) %in% "traila")
cn1 <- cn[-which(cn %in% c(cn2, cn3))]


#info for complete.cases
merged2 <- merged[, cn1]
#complete cases doesn't consider missingness/ complete case



cn4 <- which(substr(colnames(merged), 3, 5) == "mmt")
cn1 <- cn[-which(cn %in% cn2)]
cn1 <- c(cn1, cn4)

merged2 <- merged[, cn1]


#rename columns
cn <- colnames(merged2)
wh1 <- which(substr(cn, 3, 5) == "slo")
cn[wh1] <- paste0("ws", seq(1, 6))
wh1 <- which(cn == "scrgenhl")
cn[wh1] <- "health"
wh1 <- which(cn == "scrptrc3")
cn[wh1] <- "race"
wh1 <- which(cn == "scrpted2")
cn[wh1] <- "ed"
wh1 <- which(cn == "scrptage")
cn[wh1] <- "age"
wh1 <- which(substr(cn, 3, 5) == "phy")
cn[wh1] <- paste0("sppb", c(1, 2, 3, 5, 6))
wh1 <- which(cn == "scrvsnpb")
cn[wh1] <- "vision"
wh1 <- which(cn == "dated")
cn[wh1] <- "death"
wh1 <- which(substr(cn, 3, 5) == "mmt")
cn[wh1] <- paste0("mmse", seq(1, 6))

colnames(merged2) <- cn

write.dta(merged2, file = file.path(dat.dir, "all_aging_data.dta"))



cn <- c("trailb1", "ws1", "hvlr1", "hvldel1", "mmse1", "sppb1")
sd1 <- apply(merged2[, cn], 2, sd, na.rm = T)
sd1["hvldel1"] <- sd(merged2$hvldel1^2, na.rm = T)


save(sd1, file = file.path(dat.dir, "all_sds.RData"))



########
# checks against old

#check sppb
# j <- 1
# for(i in c(1, 2, 3, 5, 6)) {
	# name1 <- paste0("r", i, "phypermtot")
	# print(all.equal(merged[, name1], olddat[, 23 + j]))
	# j <- j + 1
# }



# #check ta, tb, hvlr, hvldel, ws
# oldseq <- c(18, 12, 34, 40, 6)
# names1 <- c("traila", "trailb", "hvlr", "hvldel", "r")
# for(i in 1 : length(oldseq)) {
	# for(j in 1 : 6) {
		# names2 <- paste0(names1[i], j)
		# if(i == 5) {
			# names2 <- paste0(names2, "slow")
		# }
		# print(names2)
		# print(all.equal(olddat[, oldseq[i] - 1 + j], 
			# merged[, names2]))
		
	# }
# }
# for(j in 1 : 6) {
	# names2 <- paste0("hvldel", j)
	# print(all.equal(olddat[, 46 - 1 + j], 
		# merged[, names2]^2))
# }





######
#check with old
# visdates2 <- visdates[which(visdates[, 1] %in% olddat[, 1]), ]
# visdates2 <- visdates2[order(visdates2[, 1]), ]
# for(i in 1 : 5) {
	# print(all.equal(visdates2[, i + 1], olddat[, 28 + i]))
# }




# #check death
# dates for visit, but no measurements
# wh999 <- which(merged$rmvis2 == 999)
# for(i in 1 : length(wh999)) {
	# visit <- merged$rmvis[wh999[i]]
	# gr <- paste0("[", visit, ":6]")
	# print(merged[wh999[i], grep(gr, colnames(merged))])
# }






# all.equal(merged$scrptage, olddat[, 2])
# all.equal(merged$scrptrc3, olddat[, 5])
# all.equal(merged$scrpted2, olddat[, 4])
# all.equal(merged$scrgenhl, olddat[, 3])
# all.equal(merged$disease, olddat[, 52])
# all.equal(merged$gds, as.numeric(as.character(olddat[, 53])))
# all.equal(merged$gdsc, as.numeric(as.character(olddat[, 54])))



#check years of data
library(foreign)
dat <- read.dta(file.path(dat.dir, "mstat7_w2.dta"))
dates <- c(paste0("r", seq(1, 7), "date"), "lastdate", "r1cedate", "lcondate")
dat <- dat[, dates]
apply(dat, 2, min, na.rm = T)
apply(dat, 2, max, na.rm = T)
#1994 is earliest date
#last r7 date-- 2009?  last r6 date, 2006