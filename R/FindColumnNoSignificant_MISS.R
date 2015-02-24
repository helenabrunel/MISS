FindColumnNoSignificant_MISS <- function(oFdata, iFsel=iFsel, ophen,  reps=3000, cl=NULL)
{
	pos <- iFsel
	#pos <- iFsel[c(1:(length(iFsel)-1))]
	MI<-matrix(,1,length(pos))
	pval<-matrix(,1,length(pos))


	if ( is.null(cl) ) 
	{ 
		out <- sapply(X=1:length(pos), FUN=isNoSignif_MISS, oFdata=oFdata, ophen=ophen, pos=pos, iFsel=iFsel,reps=reps)
	} else {
		out <- parSapply(cl, X=1:length(pos), FUN=isNoSignif_MISS, oFdata=oFdata, ophen=ophen, pos=pos, iFsel=iFsel,reps=reps)
	}
	

	#MI <- unlist(out["MI",])
	#pval <- unlist(out["pval",])
	#data.frame(MI=(MI),pval=(pval),pos=(pos))
	data.frame(pval=(out),pos=(pos))
}
