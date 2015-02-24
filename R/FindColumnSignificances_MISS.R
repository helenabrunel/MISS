FindColumnSignificances_MISS <- function(oFdata, iFsel=c(), ophen,  reps=3000, cl=NULL)
{
	pos <- setdiff(1:ncol(oFdata),iFsel)
	MI<-matrix(,1,length(pos))
	pval<-matrix(,1,length(pos))
	if ( is.null(cl) ) 
	{ 
		out <- sapply(X=1:length(pos), FUN=isSignif_MISS, oFdata=oFdata, ophen=ophen, pos=pos, iFsel=iFsel,reps=reps)
	} else {
		out <- parSapply(cl, X=1:length(pos), FUN=isSignif_MISS, oFdata=oFdata, ophen=ophen, pos=pos, iFsel=iFsel, reps=reps)
	}
	data.frame(pval=(out),pos=(pos))
	
	#MI <- unlist(out["MI",])
	#pval <- unlist(out["pval",])
	#data.frame(MI=(MI),pval=(pval),pos=(pos))
}
