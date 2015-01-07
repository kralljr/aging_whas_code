
######## 
#######
# GET PVALUES FOR LRT TESTS OF PARAMETER CHANGES ACROSS TIMES
######## 
#######


# # nameTBWS, nameHVLRWS, nameTBSP, nameHVLRSP,
	# nameSQHVLDELWS, nameSQHVLDELSP
	
	
# getLRTpval(nameTBWS, tmt.dir, "WS_tmtb_testtrend_ARTB_18dec12.out")

#HVL-imm and WS
hvlrws <- getall(nameHVLRWS, hvl.dir, "testtrend/WS_hvlr_testrend_",
	"_18dec12.out","HVL", "WS")
#HVL-imm and SP
hvlrsp <- getall(nameHVLRSP, hvl.dir, "testtrend/SPPB_hvlr_testtrend_",
	"_18dec12.out","HVL", "SP")
#HVL-del and WS
hvldelws <- getall(nameSQHVLDELWS, hvl.dir, "testtrend/WS_sqhvldel_testtrend_",
	"_18dec12.out","HVL", "WS")
#HVL-del and SP
hvldelsp <- getall(nameSQHVLDELSP, hvl.dir, "testtrend/SPPB_sqhvldel_testtrend_",
	"_18dec12.out","HVL", "SP")
	

	
#TMT-B and WS	
tmtbws <- getall(nameTBWS, tmt.dir, "WS_tmtb_testtrend_",
	"_18dec12.out","TB", "WS", typeimp = "impute")
#TMT-B and SP
tmtbsp <- getall(nameTBSP, tmt.dir, "sppb_tmtb_testtrend_",
	"_18dec12.out","TB", "SP", typeimp = "impute")
	
rbind(tmtbws, hvlrws, hvldelws)	
rbind(tmtbsp, hvlrsp, hvldelsp)	






##########MMSE
#MMSE and WS
mmsews <- getall(nameMMSEWS, hvl.dir, "WS_MMSE_testtrend_",
	"_9apr13.out","MMSE", "WS")
#MMSE and SP
mmsesp <- getall(nameMMSESP, hvl.dir, "SPPB_MMSE_testtrend_",
	"_9apr13.out","MMSE", "SP")		
mmsewsc <- getall(nameMMSEWS, hvl.dir, "WS_MMSE_censor_testtrend_",
	"_9apr13.out","MMSE", "WS")
#MMSE and SP
mmsespc <- getall(nameMMSESP, hvl.dir, "SPPB_MMSE_censor_testtrend_",
	"_9apr13.out","MMSE", "SP")	
mmsews
mmsewsc
mmsesp
mmsespc




######## GET LRT TESTS FOR PARTIAL CONSTRAIN
#MMSE/WS
getLRTpval(file.path(hvl.dir, "ws_mmse_constrain2_17apr13.out"), hvl.dir, 
	"ws_mmse_constrain_17apr13.out", type = "no") #not sig
#MMSE/SPPB
getLRTpval(file.path(hvl.dir, "sppb_mmse_constrain2_17apr13.out"), hvl.dir, 
	"sppb_mmse_constrain_17apr13.out", type = "no") #not sig	
	
#HVL-imm/WS
#not constrained, vs. constrain---- ALLOW ALL DIFFERENT
getLRTpval(file.path(hvl.dir, "ws_hvlr_constrain2_16apr13.out"), hvl.dir, 
	"ws_hvlr_constrain4_16apr13.out", type = "no") #not sig
getLRTpval(file.path(hvl.dir, "ws_traila_trailb_time_hvlr_21aug12.out"), hvl.dir, 
	"ws_hvlr_constrain3_16apr13.out", type = "no") #not sig

		
#HVL-imm/SPPB		
getLRTpval(file.path(hvl.dir, "sppb_hvlr_constrain2_16apr13.out"), hvl.dir, 
	"sppb_hvlr_constrain_16apr13.out", type = "no") #not sig
getLRTpval(file.path(hvl.dir, "sppb_hvlr_constrain3_16apr13.out"), hvl.dir, 
	"sppb_hvlr_constrain_16apr13.out", type = "no") #not sig
	
	
#HVL-del/WS
getLRTpval(file.path(hvl.dir, "ws_sqhvldel_constrain2_16apr13.out"), hvl.dir, 
	"ws_sqhvldel_constrain_16apr13.out", type = "no") #not sig
getLRTpval(file.path(hvl.dir, "ws_sqhvldel_constrain3_16apr13.out"), hvl.dir, 
	"ws_sqhvldel_constrain_16apr13.out", type = "no") #sig, p = 0.033, 2 = 3= 4
	
#HVL-del/SPPB
getLRTpval(file.path(hvl.dir, "sppb_sqhvldel_constrain2_16apr13.out"), hvl.dir, 
	"sppb_sqhvldel_constrain_16apr13.out", type = "no") #not sig
getLRTpval(file.path(hvl.dir, "sppb_sqhvldel_constrain3_16apr13.out"), hvl.dir, 
	"sppb_sqhvldel_constrain_16apr13.out", type = "no") #not sig
	
#TMT-b/WS
getLRTpval(file.path(tmt.dir, "ws_traila_trailb_time_constrain2_16apr13.out"), tmt.dir, 
	"ws_traila_trailb_time_constrain_16apr13.out", type = "impute") #not sig
getLRTpval(file.path(tmt.dir, "ws_traila_trailb_time_constrain3_16apr13.out"), tmt.dir, 
	"ws_traila_trailb_time_constrain_16apr13.out", type = "impute") #not sig
#TMT-B/SPPB	
getLRTpval(file.path(tmt.dir, "sppb_traila_trailb_time_constrain2_16apr13.out"), 
	tmt.dir, "sppb_traila_trailb_time_constrain_16apr13.out", 
	type = "impute") #sig p = 0.033
	
#ONLY SIG FOR 
#TMT-b -> SPPB 1/2 different from 3/4
#HVL-del ->ws 2=3=4 and 1=5
	




#####LRT FOR AR EFFECTS OF FINAL, Crosslagged Constrained models (compared to constraining AR effects)
#TB/WS
getLRTpval(file.path(tmt.dir, "ws_traila_trailb_time_constrain_16apr13.out"), tmt.dir, 
	"ws_traila_trailb_time_constrainARWS_16apr13.out", type = "impute") #sig p <0.001
getLRTpval(file.path(tmt.dir, "ws_traila_trailb_time_constrain_16apr13.out"), tmt.dir, 
	"ws_traila_trailb_time_constrainARTB_16apr13.out", type = "impute") #sig p =0.002
#TB/SP	
getLRTpval(file.path(tmt.dir, "sppb_traila_trailb_time_constrain_16apr13.out"), tmt.dir, 
	"sppb_traila_trailb_time_constrainARSP_16apr13.out", type = "impute") #not sig p =0.090
getLRTpval(file.path(tmt.dir, "sppb_traila_trailb_time_constrain_16apr13.out"), tmt.dir, 
	"sppb_traila_trailb_time_constrainARTB_16apr13.out", type = "impute") #sig p <0.001
	
	
#HVLimm/WS
getLRTpval(file.path(hvl.dir, "ws_hvlr_constrain_16apr13.out"), hvl.dir, 
	"ws_hvlr_constrainARWS_16apr13.out", type = "no") #not sig p <0.001
getLRTpval(file.path(hvl.dir, "ws_hvlr_constrain_16apr13.out"), hvl.dir, 
	"ws_hvlr_constrainARHV_16apr13.out", type = "no") #not sig p = 0.017
#HVLimm/SP
getLRTpval(file.path(hvl.dir, "sppb_hvlr_constrain_16apr13.out"), hvl.dir, 
	"sppb_hvlr_constrainARSP_16apr13.out", type = "no") #not sig p 0.070
getLRTpval(file.path(hvl.dir, "sppb_hvlr_constrain_16apr13.out"), hvl.dir, 
	"sppb_hvlr_constrainARHV_16apr13.out", type = "no") #not sig p = 0.029

#HVLdel/WS
getLRTpval(file.path(hvl.dir, "ws_sqhvldel_constrain_16apr13.out"), hvl.dir, 
	"ws_sqhvldel_constrainARWS_16apr13.out", type = "no") #not sig p <0.001
getLRTpval(file.path(hvl.dir, "ws_sqhvldel_constrain_16apr13.out"), hvl.dir, 
	"ws_sqhvldel_constrainARHV_16apr13.out", type = "no") #not sig p = 0.230
#HVLdel/SP		
getLRTpval(file.path(hvl.dir, "sppb_sqhvldel_constrain_16apr13.out"), hvl.dir, 
	"sppb_sqhvldel_constrainARSP_16apr13.out", type = "no") #not sig p = 0.076
getLRTpval(file.path(hvl.dir, "sppb_sqhvldel_constrain_16apr13.out"), hvl.dir, 
	"sppb_sqhvldel_constrainARHV_16apr13.out", type = "no") #not sig p = 0.258
	
#MMSE/WS
getLRTpval(file.path(hvl.dir, "ws_mmse_constrain_17apr13.out"), hvl.dir, 
	"ws_mmse_constrainARWS_17apr13.out", type = "no") #sig p< 0.001
getLRTpval(file.path(hvl.dir, "ws_mmse_constrain_17apr13.out"), hvl.dir, 
	"ws_mmse_constrainARHV_17apr13.out", type = "no") #sig p=0.042	
#MMSE/SPPB
getLRTpval(file.path(hvl.dir, "sppb_mmse_constrain_17apr13.out"), hvl.dir, 
	"sppb_mmse_constrainARSP_17apr13.out", type = "no") #not sig p=0.101	
getLRTpval(file.path(hvl.dir, "sppb_mmse_constrain_17apr13.out"), hvl.dir, 
	"sppb_mmse_constrainARMM_17apr13.out", type = "no") #sig	p = 0.014		
	
	
	
	