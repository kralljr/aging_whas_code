#file to get ratio of parameters



ratfun <- function(name1,name2,typep){
			
	par1 <- extractModelParameters(name1)
	par2 <- extractModelParameters(name2)
	parcheck1 <- fixdat(par1)
	parcheck2 <- fixdat(par2)	
	
	estsrat <- c(0,0,0,0)
	
	
	typey <- c(rep(typep,3),"TB","TB","TB")
	typex <- rep(c(typep,"TB","TA"),2)
	for(i in 1:6){
		ests1 <- regs(parcheck1,typey[i],typex[i])
		ests2 <- regs(parcheck2,typey[i],typex[i])
#		dats <- data.frame(ests1[,c(1,2)],(ests1[,c(3,4)]-ests2[,c(3,4)])/ests1[,c(3,4)])
	dats <- data.frame(ests1[,c(1,2)],(ests1[,c(3,4)]/ests2[,c(3,4)]))
		estsrat <- rbind(estsrat,dats)
	}
	estsrat <- estsrat[-1,]
	estsrat
}