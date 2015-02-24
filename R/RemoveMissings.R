RemoveMissings <- function(genos,phenos=NULL)
{
	genos<-as.matrix(genos)
	inozerosgenos <- which(apply(is.na(genos),1,sum)==0)
	inozerosphenos <- c(1:nrow(genos))
	if (!is.null(phenos))
	inozerosphenos <- which(!is.na(phenos))

	# no missingd at genotypes, nor at phenotypes
	sort(intersect(inozerosgenos,inozerosphenos))
}
