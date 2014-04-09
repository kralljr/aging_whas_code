library(ggplot2)

stripests <- function(ge) {
	
	rn <- rownames(ge[[2]])
	ge <- as.vector(as.character(ge[[2]][, 1]))
	estsCI <- strsplit(ge, "\\(")
	ests <- sapply(estsCI, function(x) as.numeric(x[[1]]))
	CI <- strsplit(sapply(estsCI, function(x) x[[2]]), ",")
	lb <- sapply(CI, function(x) as.numeric(x[[1]]))
	ub <- as.numeric(unlist(strsplit(sapply(CI, 
		function(x)  x[[2]]), "\\)")))
	out <- data.frame(ests, lb, ub)
	colnames(out) <- c("est", "lb", "ub")
	
	rownames(out) <- rn
	out
}

getestsout <- function(name1, name2, typex, typey, ns, phys, cog) {
	dat1 <- stripests(getests(name1, typex, typey))
	dat2 <- stripests(getests(name2, typex, typey))
	
	others <- rep(ns, each = nrow(dat1))
	time <- rep(seq(1, nrow(dat1)), 2)
	phys <- rep(phys, length(time))
	cog <- rep(cog, length(time))
	
	dat <- rbind(dat1, dat2)
	out <- data.frame(dat, others, time, phys, cog)
	colnames(out) <- c("est", "lb", "ub", "type", "time", "phys", "cog")
	out
}


# out <- getestsout(con[1], ageedrace[1], "TB", "WS", "AER:constrain", "AER:free")



ggplotWHAS <- function(names1, names2, ns, type, cog = NULL, phys = NULL) {
	
	if(is.null(cog) | is.null(phys)) {
		phys <- c("WS", "SP")
		cog <- c("TB", "HV", "SH", "MM")
	}
	k <- 1
	for(i in 1 : length(phys)) {	
		for(j in 1 : length(cog)) {
			if(type == "ARC") {
				typex <- cog[j]
				typey <- cog[j]
			}else if(type == "ARP") {
				typex <- phys[i]
				typey <- phys[i]
			}else if(type == "CLP") {
				typex <- cog[j]
				typey <- phys[i]
			}else if(type == "CLC") {
				typex <- phys[i]
				typey <- cog[j]
			}
			
			
			if(k == 1) {
				out <- getestsout(names1[k], names2[k], typex, 
					typey, ns, phys[i], cog[j])
			}else{
				out1 <- getestsout(names1[k], names2[k], typex, 
					typey, ns, phys[i], cog[j])
				out <- rbind(out, out1)
				}
			k <- k + 1
		}
	}
	
	pd <- position_dodge(.4)
	size1 <- 18
	sizeline <- 0.8
	g1 <- ggplot(out, aes(x = time, y = est, colour = type)) +
	geom_hline(aes(yintercept = 0), colour = "grey80", 
		linetype = "dashed", size = sizeline) +
    geom_pointrange(aes(ymin = lb, ymax = ub, colour = type), 
  	  width = 0.1, position = pd, size = sizeline) +
  	  
    scale_colour_hue(drop = FALSE, name="",
                     breaks=c(ns[1], ns[2]),
                     # labels=c("Lag 0", "Lag 1", "Lag 2"),
                     l=40) + 
    ylab(expression(atop("increase in SD per increase in SD")))
    g1 <- g1 + facet_grid(phys~ cog)
	list(g1, out)
}


# ggplotWHAS(con, ageedrace, c("constrain:CL", "uncon"), "ARC")


compareALL <- function(names1, names2, ns, cog = NULL, phys = NULL) {
	
	
	types <- c("ARC", "ARP", "CLC", "CLP")
	gout <- list()
	for(i in 1 : 4) {
		gout[[i]] <- ggplotWHAS(names1, names2, ns, types[i], cog, phys)			
	}
	gout
}



# # g2 <- compareALL(con, ageedrace, c("constrain:CL", "uncon"))
# pdf("constrain_CL.pdf")
# g2[[1]][[1]]
# g2[[2]][[1]]
# g2[[3]][[1]]
# g2[[4]][[1]]
# graphics.off()


