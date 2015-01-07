
getpars <- function(name1, typey, typex){
  
  
  par <- extractModelParameters(name1)
  parcheck <- fixdat(par,std=FALSE)

  ests <- regs(parcheck,typey,typex)
  lb <- (ests$est-1.96*ests$se) 
  ub <- (ests$est+1.96*ests$se)
  ests <- ests$est 
  
  
  ests2out <- cbind(ests,lb,ub)
  if(length(ests) == 4){
    
    ests2out <- rbind(ests2out[1:2,], rep(NA,3), ests2out[3:4,])
    
  }
  ests2out
}	




plotAR <- function(typeAR, ylims, labs,
                   pch2 = 17, ltys = 1, lwds = 1.5, caxis = 1.5, 
                   cexpt = 2,
                  cexlab = 1.5, legcor, xlims =  c(0.25, 5.75)) {
  par(oma=c(1, 2, 1, 1)) 
  par(mar=c(5,4.5,4,2) + 0.1)   
  ltys1 <- c(1, 3, 5, 1, 3, 5, 4, 4)
  ltys1 <- c(1, 3, 5, 4, 1, 3, 5, 4)
  pchs1 <- c(16, 17, 15, 18, 1, 2, 22, 23)
  if(typeAR %in% c("WS", "SP")) {
    others <- c("TB", "HVLR", "HVLD", "MM")
    if(typeAR == "WS") {
      keeps <- seq(1, 4)
      label <- "WS"
    }else{
      keeps <- seq(5, 8)
      label <- "SPPB"
    }
  }else{
    others <- c("WS", "SP")
    if(typeAR == "TB") {
      keeps <- c(1, 5)
      label <- "TMT-B"
    }else if(typeAR == "HVLR") {
      keeps <- c(2, 6)
      typeAR <- "HV"
      label <- "HVLT-imm"
    }else if(typeAR == "HVLD") {
      keeps <- c(3, 7)
      typeAR <- "SH"
      label <- "HVLT-del"
    }else if(typeAR == "MM") {
      keeps <- c(4, 8)
      label <- "MMSE"
    }
  }
  names <- namef[keeps]
  pchs <- pchs1[keeps]
  ltys <- ltys1[keeps]
  
  seqs <- seq(1, 5)
  par(bty = "l")
  plot(1, 1, xlim = xlims, ylim = ylims, 
       pch=pch2, main = "", ylab = "",
       cex.lab = cexlab, cex = cexpt, cex.axis = caxis, 
       type = "n", las = 1, bty = "l", xaxt = "n",
       xlab = "", xaxs = "i", yaxs = "i")
  mtext("Visit", side = 1, at = 3, cex = caxis,
        line = 3)
  if(label != "HVLT-del") {
    
    mtext(bquote(Change~"in"~.(label)~at~Visit~italic(t)),  
          side = 2, cex = caxis, line = 4)
  }else{
    mtext(expression("Change in "* "HVLT-del"^"2" * " at Visit " *italic(t)), 
          side = 2, cex = caxis, line = 4)
    
  }
  
  mtext(3, ylims[2] + .2, text = labs, 
        cex = caxis, outer = T, at = 0, line = -2)
  
  axis(1, at = seqs, labels = seqs + 1, cex.axis= caxis)
  ns <- length(others)
  if(ns == 4) {
    jit <- seq(-0.3, 0.3, length = ns)
  }else{
    jit <- c(-0.1, 0.1)
  }
  
  
  for(i in 1 : ns) { 
    dat <- getpars(names[i], typeAR, typeAR)
    
    xs <- seqs + jit[i] 
    points(xs, dat[, 1], pch = pchs[i], cex = cexpt)
    segments(xs, dat[, 2], y1 = dat[, 3], lty = ltys[i], lwd = lwds)
    segments(xs - 0.05, dat[, 2], xs + 0.05, dat[, 2], lwd = lwds)
    segments(xs - 0.05, dat[, 3], xs + 0.05, dat[, 3], lwd = lwds)
  }
  
  
  legs <- c("WS/TMT-B", "WS/HVLT-imm", 
            "WS/HVLT-del", "WS/MMSE",
            "SPPB/TMT-B", "SPPB/HVLT-imm", 
            "SPPB/HVLT-del", "SPPB/MMSE")
  legend(legcor[1], legcor[2], legs[keeps], cex = caxis , pch = pchs,
         lty = ltys, lwd = lwds)
  
}


ylims <- c(0, 1)
legcor1 <- c(3.4, 0.325)
xlims1 <-  c(0.25, 6.2)



setwd("C:/Users/jrkrall/Dropbox/Aging/proposal/writeup/AJE_techreview/Figurespdf")

pdf("Figure3a.pdf", height = 7, width = 8)
plotAR("WS", ylims, labs = c("A)"), legcor = legcor1, xlims = xlims1)
graphics.off()

pdf("Figure3b.pdf", height = 7, width = 8)
plotAR("SP", ylims, labs = c("B)"), legcor = legcor1, xlims = xlims1)
graphics.off()




legcor2 <- c(1.9, 0.2)

pdf("Figure2a.pdf", height = 7, width = 8)
plotAR("TB", ylims, labs = c("A)"), legcor = legcor2)
graphics.off()


legcor2 <- c(1.725, 0.2)


pdf("Figure2b.pdf", height = 7, width = 8)
plotAR("HVLR", ylims, labs = c("B)"), legcor = legcor2)
graphics.off()

legcor2 <- c(1.775, 0.2)



pdf("Figure2c.pdf", height = 7, width = 8)
plotAR("HVLD", ylims, labs = c("C)"), legcor = legcor2)
graphics.off()

legcor2 <- c(1.95, 0.2)

pdf("Figure2d.pdf", height = 7, width = 8)
plotAR("MM", ylims, labs = c("D)"), legcor = legcor2)
graphics.off()