#file to create pdfs for GSA 2012
# 5nov12


home.dir <- "/Users/jennakrall/Dropbox/Aging/"
moddir <- file.path(home.dir, "final_models")
setwd(file.path(home.dir, "plots"))


source(file.path(home.dir, "aging_code", "parsing_mplus_files.R"))
source(file.path(home.dir, "aging_code", "compareres_5jul12.R"))



library(RColorBrewer)
library(colorspace)
library(lattice)




###########
#get output from Mplus
name1 <- file.path(moddir, "ws_traila_trailb_time_constrain_16apr13.out")
name2 <- file.path(moddir, "sppb_traila_trailb_time_constrain_16apr13.out")
name3 <- file.path(moddir, "ws_hvlr_constrain_16apr13.out")
name4 <- file.path(moddir, "sppb_hvlr_constrain_16apr13.out")
name5 <- file.path(moddir, "ws_sqhvldel_constrain_16apr13.out")
name6 <- file.path(moddir, "sppb_sqhvldel_constrain_16apr13.out")
name7 <-file.path(moddir, "ws_mmse_constrain_17apr13.out")
name8 <- file.path(moddir, "sppb_mmse_constrain_17apr13.out")

	
	
	


# colsall <- brewer.pal(3, "Dark2")
colsall <- rep(1, 5)


# cols <- brewer.pal(6, "Dark2")
# cols <- c("grey20", 1, "grey50", "grey80", "grey40", 1, "grey80", "grey50")
cols <- rep(1, 8)
ltys <- c(1, 1, 1, 2, 2, 2, 1, 2)
ltys <- c(1, 3, 5, 1, 3, 5, 4, 4)

# ltys <- rep(c(1 , 2), each = 4)
pchs <- c(16, 2, 15, 23, 1, 17, 22, 18)
pchs <- c(16, 17, 15, 18, 1, 2, 22, 23)


pdf("Paper_ARPHYS_finalconstrain_25apr13.pdf", height = 7, width = 13)

#for color/ppt
# cp <- 4
# cl <- 3
# ca <- 4
# cleg <- 2.5

#
#for b&w/paper
cp <- 2
cl <- 1.5
ca <- 1.5
cleg <- 1.5
# par(mfrow = c(1,3), mar = c(6,5,5,0), oma = c(0, 3, 0, 0))
par(mfrow = c(1,2), mar = c(6,4,5,0), oma = c(0, 6, 0, 0))
plotsidebyside(name1, name2, "WS", "WS","SP", "SP", c(0,.9), c(1.5,6.5),
	"a. Walking speed",
	"b. SPPB", 
	"previous phys", "phys", pch2 = pchs[1], ltys = ltys[1], lwds = 1, col2 = cols[1],
	axis1 = T,cexlab = cl, cexpt = cp, caxis = ca)
	# mtext(paste("AR effects of physical function", sep=""), 
			# side = 2, line = 0, cex = 3, outer = T)	
	mtext(expression(atop("Change in baseline sd of x"["t"]) ,"for baseline sd increase in x"["t-1"]), 
			side = 2, line = -1, cex = 1.5, outer = T)		
plotsidebyside(name3, name4, "WS", "WS","SP", "SP", c(0.2,.8), c(1.5,6.5),
	"a. Walking speed",
	"b. SPPB", 
	"previous phys", "phys", pch2 = pchs[2], plot = FALSE, seqs = seq(2.2, 6.2),
	col2 = cols[2], ltys = ltys[2],cexlab = cl, cexpt = cp, caxis = ca, lwds =1)	
plotsidebyside(name5, name6, "WS", "WS","SP", "SP", c(0.2,.8), c(1.5,6.5),
	"a. Walking speed",
	"b. SPPB", 
	"previous phys", "phys", pch2 = pchs[3], plot = FALSE, seqs = seq(2.4, 6.4),
	col2 = cols[3], ltys = ltys[3], lwds = 1,cexlab = cl, cexpt = cp, caxis = ca)		
plotsidebyside(name7, name8, "WS", "WS","SP", "SP", c(0.2,.8), c(1.5,6.5),
	"a. Walking speed",
	"b. SPPB", 
	"previous phys", "phys", pch2 = pchs[4], plot = FALSE, seqs = seq(2.6, 6.6),
	col2 = cols[7], ltys = ltys[7], lwds = 1,cexlab = cl, cexpt = cp, caxis = ca)		
	
	legend("bottomright", col = cols[c(1:3, 7)], lty = ltys[c(1:3, 7)],
	lwd = rep(1, 4), pch = pchs[1:4], legend = c("(i): TMT-B",
	"(iii): HVLT-imm",  expression(paste("(v): HVLT-del"^2)) , "(vii): MMSE") , cex = cleg)
#"(i): TMT-B, p<0.01",
	# "(iii): HVL-imm, p<0.01", "(v): HVL-del^2, p=?"	
	

	
par(mar = c(6,0,5,4))		
plotsidebyside(name1, name2, "WS", "WS","SP", "SP", c(0,.9), c(1.5,6.5),
	"a. Walking speed",
	"b. SPPB", 
	"previous phys", "phys", pch2 = pchs[5], plot2 = TRUE, ltys = ltys[4], lwds = 1,
	col2 = cols[4], axis1 = T,cexlab = cl, cexpt = cp, caxis = ca)	
plotsidebyside(name3, name4, "WS", "WS","SP", "SP", c(0.2,.8), c(1.5,6.5),
	"a. Walking speed",
	"b. SPPB", 
	"previous phys", "phys", pch2 = pchs[6], plot = FALSE, seqs = seq(2.2, 6.2),
	col2 = cols[5], plot2 = TRUE, ltys = ltys[5],
	cexlab = cl, cexpt = cp, caxis = ca, lwds = 1)	
plotsidebyside(name5, name6, "WS", "WS","SP", "SP", c(0.2,.8), c(1.5,6.5),
	"a. Walking speed",
	"b. SPPB", 
	"previous phys", "phys", pch2 = pchs[7], plot = FALSE, seqs = seq(2.4, 6.4),
	col2 =  cols[6], plot2 = TRUE, ltys = ltys[6], lwds = 1, 
	cexlab = cl, cexpt = cp, caxis = ca)		
plotsidebyside(name7, name8, "WS", "WS","SP", "SP", c(0.2,.8), c(1.5,6.5),
	"a. Walking speed",
	"b. SPPB", 
	"previous phys", "phys", pch2 = pchs[8], plot = FALSE, seqs = seq(2.6, 6.6),
	col2 =  cols[8], plot2 = TRUE, ltys = ltys[8], lwds = 1, 
	cexlab = cl, cexpt = cp, caxis = ca)		
	
	
	# lwd = c(2, 3, 4, 2)
		
	legend("bottomright", col = cols[c(4:6, 8)], lty = ltys[c(4:6, 8)],
	lwd = rep(1, 4), pch = pchs[5:8], legend = c("(ii): TMT-B",
	"(iv): HVLT-imm", expression(paste("(vi): HVLT-del"^2)), "(viii): MMSE" ), 
	cex = cleg)
	# "(ii): TMT-B, p=0.07",
	# "(iv): HVL-imm, p=0.04", "(vi): HVL-del^2, p=0.06"
# graphics.off()

# plot(1, 1, type = "n", axes = F, xlab = "", ylab = "")
graphics.off()







# #AR effects of cog
# pdf("Paper_ARcog_color_10feb12.pdf",height=7, width=15)
# pdf("Paper_ARcog_10feb12.pdf",height=7, width=15)
# pdf("Paper_ARCOG_constrain_16apr13.pdf", height = 7, width = 15)
pdf("Paper_ARCOG_finalconstrain_25apr13.pdf", height = 9, width = 9)

# cp <- 4
# cl <- 3
# ca <- 4
# cleg <- 3
# par(mfrow = c(1,3), mar = c(5,4,6,0), oma = c(1, 2, 1, 0))
# par(mfrow = c(1, 3), mar = c(6, 5, 5, 0), oma = c(0, 3, 0, 0 ))
par(mfrow = c(2, 2), mar = c(2, 4, 3, 0), oma = c(1, 6, 0, 0 ))
plotsidebyside(name1, name2,  "TB", "TB","TB", "TB", c(0,1), c(1.5,6.5),
	"",
	"b. HVL-imm", 
	"previous cog", "cog",  pch2 = pchs[1], ltys = ltys[1], lwds = 1, col2 = cols[1],
	cexlab = cl, cexpt = cp, caxis = ca, axis1 = F)
# mtext(paste("AR effects of cognitive function", sep=""), 
			# side = 2, line = 0, cex = 3, outer = T)		
				mtext(expression(atop("Change in baseline sd of y"["t"]) ,"for baseline sd increase in y"["t-1"]), 
			side = 2, line = -1, cex = 1.5, outer = T)		
plotsidebyside(name1, name2,  "TB", "TB","TB", "TB",c(0.2,.8), c(1.5,6.5),
	"",
	"b. HVL-imm", 
	"previous cog", "cog", pch2 = pchs[5], plot = FALSE,plot2 = T, 
	seqs = seq(2.2, 6.2),
	col2 = cols[4], ltys = ltys[4],cexlab = cl, cexpt = cp, caxis = ca, lwds = 1)
legend("bottomleft", col = cols[c(1, 4)], lty = ltys[c(1, 4)],
	lwd = c(1,1), pch = c(pchs[1], pchs[5]), legend = c("(i): WS",
	"(ii): SPPB"), cex = cleg)
	
mtext("a. TMT-B", cex = ca)	
		

par(mar = c(2,0,3,4))		
plotsidebyside(name4, name3,  "HV", "HV","HV", "HV", c(0,1), c(1.5,6.5),
	"a. TMT-B",
	"", 
	"previous cog", "cog", pch2 = pchs[2], plot2 = TRUE, ltys = ltys[2], lwds = 1,
	col2 = cols[2],cexlab = cl, cexpt = cp, caxis = ca, axis1 = F)		
plotsidebyside(name3, name4,  "HV", "HV","HV", "HV", c(0.2,.8), c(1.5,6.5),
	"a. TMT-B",
	"b. HVL-imm", 
	"previous cog", "cog",  pch2 = pchs[6], plot = FALSE, seqs = seq(2.2, 6.2),
	col2 = cols[5], plot2 = TRUE, ltys = ltys[5],cexlab = cl, cexpt = cp, caxis = ca,
	lwds = 1)	
legend("bottomleft", col = cols[c(2, 5)], lty = ltys[c(2, 5)],
	lwd = c(1, 1), pch = c(pchs[2], pchs[6]), legend = c("(iii): WS",
	"(iv): SPPB"), cex = cleg)
	
mtext("b. HVLT-imm", cex = ca)		
	
par(mar = c(5,4,0,0))	
plotsidebyside(name6, name5, "HV", "HV","HV", "HV", c(0,1), c(1.5,6.5),
	"a. TMT-B",
	"", 
	"previous cog", "cog", pch2 = pchs[3], ltys = ltys[3], lwds = 1,
	col2 = cols[3],cexlab = cl, cexpt = cp, caxis = ca, plot2 = T, axis2= T)	
plotsidebyside(name5, name6,  "HV", "HV","HV", "HV",  c(0.2,.8), c(1.5,6.5),
	"a. TMT-B",
	"b. HVL-imm", 
	"previous cog", "cog",  pch2 = pchs[7], plot = FALSE, seqs = seq(2.2, 6.2),
	col2 = cols[6], plot2 = TRUE, ltys = ltys[6],
	cexlab = cl, cexpt = cp, caxis = ca, lwds = 1)	
	
legend("bottomleft", col = cols[c(3, 6)], lty = ltys[c(3, 6)],
	lwd = c(1, 1), pch = c(pchs[3], pchs[7]), legend = c("(v): WS",
	"(vi): SPPB"), cex = cleg)
	
	mtext(expression(paste("c. HVLT-del"^2)), cex = ca)	

par(mar = c(5,0,0,4))	
plotsidebyside(name8, name7, "MM", "MM","MM", "MM", c(0,1), c(1.5,6.5),
	"a. TMT-B",
	"", 
	"previous cog", "cog", pch2 = pchs[4], plot2 = TRUE, ltys = ltys[7], lwds = 1,
	col2 = cols[7],cexlab = cl, cexpt = cp, caxis = ca )	
plotsidebyside(name7, name8, "MM", "MM","MM", "MM",  c(0.2,.8), c(1.5,6.5),
	"a. TMT-B",
	"b. HVL-imm", 
	"previous cog", "cog",  pch2 = pchs[8], plot = FALSE, seqs = seq(2.2, 6.2),
	col2 = cols[8], plot2 = TRUE, ltys = ltys[8],
	cexlab = cl, cexpt = cp, caxis = ca, lwds = 1)	
	
mtext("d. MMSE", cex = ca)		
	
legend("bottomleft", col = cols[c(7, 8)], lty = ltys[c(7, 8)],
	lwd = c(1, 1), pch = c(pchs[4], pchs[8]), legend = c("(vii): WS",
	"(viii): SPPB"), cex = cleg)
				# c("WS (i, iii, v)",
	# "SPPB (ii, iv, vi)")
graphics.off()












