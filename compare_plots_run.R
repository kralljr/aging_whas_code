#######
# plots to compare




age.dir <- "/Users/jennakrall/Dropbox/Aging/"
save.dir <- "~/Dropbox/Aging/proposal/writeup/AJE_review1/"
code.dir <- file.path(age.dir, "aging_code")
mod.dir <- file.path(age.dir, "final_models")
outfile <- file.path(mod.dir, "runfiles")


source(file.path(code.dir, "compare_plots.R"))
source(file.path(code.dir, "aging_table_fn.R"))
source(file.path(code.dir, "run_mplus_models.R"))
source(file.path(code.dir, "parsing_mplus_files.R"))

physv <- c("ws", "sppb")
cogv <- c("tb", "hvlr", "shvldel", "mmse")



setwd(file.path(age.dir, "plots"))



#compare covariates age/ed/race/(TA) with 
#	covariates age/ed/race/disease/gds(TA)
# both have CL constrained
aer <- createDIR(file.path(outfile, "constrain_ageedrace"), "ageedrace_c")
dgds <- createDIR(file.path(outfile, "diseasegds"), "diseasegds")
g2 <- compareALL(aer, dgds, c("aer", "dgds"))
pdf("diseaseGDS.pdf")
g2[[1]][[1]]
g2[[2]][[1]]
g2[[3]][[1]]
g2[[4]][[1]]
graphics.off()


#USE GDS, DISEASE, ED, AGE: continuous predictors for both imputation of TMT-A and modeling, USE RACE as BINARY predictor

#NOTE: GDS WAS IMPUTED FOR ONE INDIVIDUAL




#compare covariates age/ed/race/disease/gds(TA)
#	with adding vision (binary)
# both have CL constrained
dgds <- createDIR(file.path(outfile, "diseasegds"), "diseasegds")
vision <- createDIR(file.path(outfile, "diseasegdsvision"), "diseasegds")
g2 <- compareALL(dgds, vision, c("dgds", "vision"))
pdf("diseaseGDS_addvision.pdf")
g2[[1]][[1]]
g2[[2]][[1]]
g2[[3]][[1]]
g2[[4]][[1]]
graphics.off()







#compare covariates age/ed/race/disease/gds/(TA)/vision (binary)
# with imputation
# both have CL constrained
dgds <- createDIR(file.path(outfile, "diseasegds"), "diseasegds")
vision <- createDIR(file.path(outfile, "diseasegdsvision"), "diseasegds")
g2 <- compareALL(dgds, vision, c("dgds", "vision"))
pdf("diseaseGDS_addvision.pdf")
g2[[1]][[1]]
g2[[2]][[1]]
g2[[3]][[1]]
g2[[4]][[1]]
graphics.off()


#VISION DOES NOT MAKE MUCH DIFFERENCE







#compare covariates age/ed/race/disease/gds/(TA)/vision (binary)
# with imputation
# both have CL constrained
vision <- createDIR(file.path(outfile, "diseasegdsvision"), "diseasegds")[c(1, 5)]
imputeall <- createDIR(file.path(outfile, "imputeall"), "impute_all",
	phys = c("ws", "sppb"), cog = "tb")
g2 <- compareALL(vision, imputeall, c("dgds", "vision"), 
	cog = "TB", phys = c("WS", "SP"))
	
	
pdf("allimpute.pdf")
g2[[1]][[1]]
g2[[2]][[1]]
g2[[3]][[1]]
g2[[4]][[1]]
graphics.off()








#compare covariates age/ed/race/disease/gds/(TA)/vision (binary)
# with censored
# both have CL constrained
vision <- createDIR(file.path(outfile, "diseasegdsvision"), "diseasegds")[c(6 : 8)]
censor1 <- createDIR(file.path(outfile, "censor"), "censor_sppb",
	phys = c("sppb"), cog = c("hvlr", "shvldel", "mmse"))
g2 <- compareALL(vision, censor1, c("base", "censor"), 
	cog = c("HV", "SH", "MM"), phys = "SP")

vision <- createDIR(file.path(outfile, "diseasegdsvision"), "diseasegds")[c(4, 8)]
censor1 <- createDIR(file.path(outfile, "censor"), "censor_all",
	phys = c("ws", "sppb"), cog = c("mmse"))
g3 <- compareALL(vision, censor1, c("base", "censor"), 
	cog = c("MM"), phys = c("WS", "SP"))

# # 
# vision <- createDIR(file.path(outfile, "diseasegdsvision"), "diseasegds")[c(1, 5)]
# censor1 <- createDIR(file.path(outfile, "censor"), "censor_all",
	# phys = c("ws", "sppb"), cog = c("tb"))
# g4 <- compareALL(vision, censor1, c("base", "censor"), 
	# cog = c("TB"), phys = c("WS", "SP"))



	
pdf("allcensor.pdf")
g2[[1]][[1]]
g2[[2]][[1]]
g2[[3]][[1]]
g2[[4]][[1]]
g3[[1]][[1]]
g3[[2]][[1]]
g3[[3]][[1]]
g3[[4]][[1]]
graphics.off()

#NOTE: does not appear different, checked TMT-B by hand






###########
# check constrain CLs


#check c
plusc <- createDIR(file.path(outfile, "test_cl_plusc"), "cl_plusc")

cs <- matrix(ncol = 5, nrow = length(plusc))
for(i in 1 : length(plusc)) {
	out <- extractModelParameters(plusc[i])[[1]]
	cs[i, 1 : 3] <- as.matrix(out[which(out$param == "C"), -c(1, 2, 5)])
	cs[i, 4 : 5] <- cs[i, 1] + c(-1.96, 1.96) * cs[i, 2]
	# 
}
colnames(cs) <- c("est", "se", "pval", "lb", "ub")


#check constrain
vision <- createDIR(file.path(outfile, "diseasegdsvision"), "diseasegds")
CL <- createDIR(file.path(outfile, "test_cl"), "test_cl")

pv <- vector()
for(i in 1 : length(CL)) {
	type <- "no"
	if(i == 1 | i == 5) {
		type <- "impute"
	}
	pv[i] <- getLRTpval(vision[i], CL[i], type = type)
}


cogs <- rep(c("TMT-B", "HVL-R", "(HVL-del)^2", "MMSE"), 2)
physs <- rep(c("WS", "SPPB"), each = 4)


ci <- paste0("(", round(cs[, c(4)], 2), ", ", round(cs[, 5], 2), ")")
out <- data.frame(cogs, physs, round(cs[, 1], 2),ci,  round(pv, 2))
colnames(out) <- c("Cognitive fn", "Physical fn", "Estimate", "CI", "LRT p-value")

write.csv(out, file = file.path(save.dir, "cl_diff.csv"), row.names = F)







######
# compare with residual
#compare covariates age/ed/race/disease/gds/(TA)/vision (binary)
# with imputation
# both have CL constrained
vision <- createDIR(file.path(outfile, "diseasegdsvision"), "diseasegds")
resid <- createDIR(file.path(outfile, "resid_corr"), "resid_corr")
g2 <- compareALL(vision, resid, c("vision", "resid"))
	
	
pdf("resid_corr.pdf")
g2[[1]][[1]]
g2[[2]][[1]]
g2[[3]][[1]]
g2[[4]][[1]]
graphics.off()








#####
# compare with lags
physv <- c("ws", "sppb")
cogv <- c("tb", "hvlr", "shvldel", "mmse")

vision <- createDIR(file.path(outfile, "diseasegdsvision"), "diseasegds")
dgds <- createDIR(file.path(outfile, "diseasegds"), "diseasegds")
g2 <- compareALL(vision, dgds, c("vision", "dgds"))
g3 <- lapply(g2, function(x) x[[2]])
g3 <- lapply(g3, function(x) x[which(x$type == "vision"), ])
g3 <- lapply(g3, function(x) {
	out <- data.frame(x, rep("lags1", nrow(x)))
	colnames(out)[8] <- "lag"
	out
	})


lags <- createDIR(file.path(outfile, "lags"), "lags2_all")	
dat1 <- stripests(getestsLAG(lags[1], "TB", "WS"))

cogv1 <- substr(toupper(cogv), 1, 2)
physv1 <- substr(toupper(physv), 1, 2)

reorders <- c(1, 2, 4, 6, 8, 3, 5, 7, 9)
dat <- list()
k <- 1
for(i in 1 : 2) {
	
	for(j in 1 : 4) {
		print(c(i, j))
		dat[[1]] <- stripests(getestsLAG(lags[k], cogv1[j], cogv1[j]))
		dat[[2]] <- stripests(getestsLAG(lags[k], physv1[i], physv1[i]))
		dat[[3]] <- stripests(getestsLAG(lags[k], physv1[i], cogv1[j]))
		dat[[4]] <- stripests(getestsLAG(lags[k], cogv1[j], physv1[i]))
	
		others <- lapply(dat, function(x) {
			getothers(x, "lag", cog = cogv1[j], phys = physv1[i])})
			
		#bind with others
		if(k == 1) {
			others1 <- others
		}else{
			for(l in 1 : 4) {
				others1[[l]] <- rbind(others1[[l]], others[[l]])
			}
			
		}
		k <- k + 1	
	}
}

getothers <- function(temp, type, cog, phys) {
	time1 <- as.numeric(substr(rownames(temp), 3, 3))
	time2 <- as.numeric(substr(rownames(temp), 6, 6))
	lags <- paste0("lags", time2 - time1)
	if(phys == "SP") {
		wh <- which((time2 - time1) > 2) 
		lags[wh] <- "lags2"
		
	}

	
	pp <- rep(phys, nrow(temp))
	cc <- rep(cog, nrow(temp))
	time <- as.numeric(time1)
	
	out <- data.frame(temp, rep(type, nrow(temp)), 
		time, pp, cc, lags)
	colnames(out) <- c(colnames(temp), "type", 
		"time", "phys", "cog", "lag")
	
	out
	
}


othersall <- list()
for(i in 1 : 4) {
	othersall[[i]] <- rbind(g3[[i]], others1[[i]])
}


plotlag <- function(out) {
	pd <- position_dodge(.4)
	size1 <- 18
	sizeline <- 0.8
	g1 <- ggplot(out, aes(x = time, y = est, colour = type, shape = lag)) +
	geom_hline(aes(yintercept = 0), colour = "grey80", 
		linetype = "dashed", size = sizeline) +
    geom_pointrange(aes(ymin = lb, ymax = ub, colour = type, shape = lag), 
  	  width = 0.1, position = pd, size = sizeline) +

    ylab(expression(atop("increase in SD per increase in SD")))
    
    g1 <- g1 + facet_grid(phys~ cog)
    g1
}

plotlag(othersall[[1]])


pdf("lags.pdf")
plotlag(othersall[[1]])
plotlag(othersall[[2]])
plotlag(othersall[[3]])
plotlag(othersall[[4]])
graphics.off()



