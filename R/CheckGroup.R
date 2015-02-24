CheckGroup <- function(oFdata, iFsel, file, noms = FALSE)
{
CheckGroup <- FALSE	
a <- readLines(file)
if (noms == FALSE){

	if (length(a)>=1)
		{
		for (i in 1:length(a))
			{
			grup <- as.numeric(strsplit(a[i], split=" ")[[1]])
				if(length(grup)==length(iFsel))
				{	
				if (	all(sort(grup)==sort(iFsel)))
					CheckGroup <- TRUE	
				}
			}
		}
}else {
	if (length(a)>1)
		{
		iFsel <- names(oFdata)[iFsel]
		for (i in 2:length(a))
			{
			#grup <- strsplit(a[i], split=" ")[[1]][-length(a[i])]
			grup <- strsplit(a[i], split=" ")[[1]]
				if(length(grup)==length(iFsel))
				{	
				if (	all(sort(grup)==sort(iFsel)))
					CheckGroup <- TRUE	
				}
			}
		}
}
CheckGroup
}
