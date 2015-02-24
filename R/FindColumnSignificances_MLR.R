FindColumnSignificances_MLR<- function(oFdata, iFsel=c(), ophen)
{
	pos <- setdiff(1:ncol(oFdata),iFsel)

	out <- sapply(X=1:length(pos), FUN=isSignif_MLR,oFdata=oFdata, ophen=ophen, pos=pos, iFsel=iFsel)

data.frame(pval=(out),pos=(pos))
}
