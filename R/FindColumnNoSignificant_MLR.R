FindColumnNoSignificant_MLR <-function(oFdata, iFsel, ophen) 
{
	pos <- iFsel

	out <- sapply(X=1:length(pos), FUN=isNoSignif_MLR, oFdata=oFdata, ophen=ophen, pos=pos, iFsel=iFsel)

	data.frame(pval=(out),pos=(pos))
}

