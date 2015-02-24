sffs_MLR <- function(oFdata=oFdata, iFsel=iFsel, ophen=ophen, pvalor=pvalor, file1=file1, file2=file2, file3=file3)
{
#file 1: tested groups forward
write(date(), file=file1)
#file 2: tested groups backward
write(date(), file=file2)
#file 3: selected groups
write(date(), file=file3)

#pila is a struct with 4 attributes: 
#                                  - iFsel (previously selected SNPs at each step)
#                                  - poslist (candidates to be added or removed from iFsel)
#				   - index (position in poslist)
#				   - state (indicates the sense of the search (forward or backward)) 
out <- FindColumnSignificances_MLR (oFdata=oFdata, iFsel=iFsel, ophen=ophen)
colsign <- out$pos[out$pval<pvalor]
print(colsign)
j <- 1
pila[[j]] <- list(iFsel=iFsel, poslist=colsign, index=1, state=inistate)
if (kk<- length(colsign))
	{
	while (j>0)
		{
		while (pila[[j]]$index <= length(pila[[j]]$poslist))
			{ 
			if (pila[[j]]$state=="FORWARD")
				{
				iFsel<-c(pila[[j]]$iFsel,pila[[j]]$poslist[pila[[j]]$index])
				print (pila[[j]])
				if (CheckGroup(oFdata,iFsel, file1)==FALSE)
					{
						out <- FindColumnSignificances_MLR(oFdata=oFdata, iFsel=iFsel, ophen=ophen)
						colsign <- out$pos[out$pval<pvalor]
						write(iFsel, file1, ncolumns=length(iFsel), append=TRUE)
						if (length(colsign)>0)	
							{
							j <- j+1
							pila[[j]] <- list(iFsel=iFsel, poslist=colsign, index=1, state="FORWARD")
							}else{
							if (length(iFsel)<=2)
								{
								if(CheckGroup(oFdata,iFsel, file3, noms=TRUE)==FALSE)
									{
									#write(names(oFdata)[iFsel], file3, append =TRUE)
									#Gpvalue<- GroupPvalue_MLR(iFsel, oFdata, ophen)
									#write(Gpvalue, file3, append=TRUE, ncolumns=10)
									noms <-names(oFdata)[iFsel]
									Gpvalue<- GroupPvalue_MLR(iFsel, oFdata, ophen)
									noms <- c(noms,Gpvalue)
									cat(noms, file=file3, append=TRUE)
									cat("\n", file=file3, append=TRUE)
									}
								pila[[j]]$index <- pila[[j]]$index +1
								}else{				
								if (CheckGroup(oFdata, iFsel, file2, noms=FALSE)==FALSE)
									{
									out<-FindColumnNoSignificant_MLR(oFdata=oFdata, iFsel=iFsel, ophen=ophen)
									colsign <- out$pos[out$pval>=pvalor]
									write(iFsel, file2, ncolumns=length(iFsel), append=TRUE)	
									if (length(colsign)>0){
										j<-j+1
										pila[[j]] <- list(iFsel=iFsel, poslist=colsign, index=1, state="BACKWARD")
										}else{
										if(CheckGroup(oFdata,iFsel, file3, noms=TRUE)==FALSE)
											{
											#write(names(oFdata)[iFsel], file3, append =TRUE)
											#Gpvalue<- GroupPvalue_MLR(iFsel, oFdata, ophen)
											#write(Gpvalue, file3, append=TRUE, ncolumns=10)
											noms <- names(oFdata)[iFsel]
											Gpvalue<- GroupPvalue_MLR(iFsel, oFdata, ophen)
											noms <- c(noms,Gpvalue)
											cat(noms, file=file3, append=TRUE)
											cat("\n", file=file3, append=TRUE)
											}
										pila[[j]]$index <- pila[[j]]$index+1
										} 
									}else{
									pila[[j]]$index <- pila[[j]]$index+1
									}
								}	
							}
					}else{ 
					pila[[j]]$index <- pila[[j]]$index +1
					}
				}else{
				iFsel<-setdiff(pila[[j]]$iFsel,pila[[j]]$poslist[pila[[j]]$index])
				print(pila[[j]])
				if (length(iFsel)<=2)
					{
					if (CheckGroup(oFdata,iFsel, file1)==FALSE)
						{
						out <- FindColumnSignificances_MLR(oFdata=oFdata, iFsel=iFsel, ophen=ophen)
						colsign <- out$pos[out$pval<pvalor]
						write(iFsel, file1, ncolumns=length(iFsel), append=TRUE)
						if (length(colsign)>0)	
							{
							j <- j+1
							pila[[j]] <- list(iFsel=iFsel, poslist=colsign, index=1, state="FORWARD")
							}else{
							if(CheckGroup(oFdata,iFsel, file3, noms=TRUE)==FALSE)
								{
								#write(names(oFdata)[iFsel], file3, append =TRUE)
								#Gpvalue<- GroupPvalue_MLR(iFsel, oFdata, ophen)
								#write(Gpvalue, file3, append=TRUE, ncolumns=10)
								noms <- names(oFdata)[iFsel]
								Gpvalue<- GroupPvalue_MLR(iFsel, oFdata, ophen)
								noms <- c(noms,Gpvalue)
								cat(noms, file=file3, append=TRUE)
								cat("\n", file=file3, append=TRUE)
								}
							pila[[j]]$index<-pila[[j]]$index+1
							}
						}else{
						#if(CheckGroup(oFdata,iFsel, file3, noms=TRUE)==FALSE){
						# #write(names(oFdata)[iFsel], file3, append=TRUE)
						# #Gpvalue<- GroupPvalue_MLR(iFsel, oFdata, ophen)
						# #write(Gpvalue, file3, append=TRUE, ncolumns=10)
						# noms <-names(oFdata)[iFsel]
						# Gpvalue<- GroupPvalue_MLR(iFsel, oFdata, ophen)						noms <- c(noms,Gpvalue)
						#cat(noms, file=file3, append=TRUE)
						#cat("\n", file=file3, append=TRUE)
						pila[[j]]$index<-pila[[j]]$index+1
						}
									
					}else{
					if(CheckGroup(oFdata, iFsel, file2)==FALSE)
						{
						out <- FindColumnNoSignificant_MLR(oFdata=oFdata, iFsel=iFsel, ophen=ophen)
						colsign <- out$pos[out$pval>=pvalor]
						write(iFsel, file2, ncolumns=length(iFsel), append=TRUE)
						if (length(colsign)>0)	
							{
							j<-j+1
							pila[[j]] <- list(iFsel=iFsel, poslist=colsign, index=1, state="BACKWARD")
							}else
							{	
							if (CheckGroup(oFdata, iFsel, file1)==FALSE)
								{
								out<-FindColumnSignificances_MLR(oFdata=oFdata, iFsel=iFsel, ophen=ophen)
								colsign <- out$pos[out$pval < pvalor]
								write(as.matrix(iFsel)[,1], file1, ncolumns=length(iFsel), append=TRUE)
								if(l <- length(colsign))
									{
									j <- j+1
									pila[[j]] <- list(iFsel=iFsel, poslist=colsign, index=1, state="FORWARD")
									}else{
									if(CheckGroup(oFdata,iFsel, file3, noms=TRUE)==FALSE)
										{
										#write(names(oFdata)[iFsel], file3, append =TRUE)
										#Gpvalue<- GroupPvalue_MLR(iFsel, oFdata, ophen)
										#write(Gpvalue, file3, append=TRUE, ncolumns=10)
										noms <- names(oFdata)[iFsel]
										Gpvalue<- GroupPvalue_MLR(iFsel, oFdata, ophen)
										noms <- c(noms,Gpvalue)
										cat(noms, file=file3, append=TRUE)
										cat("\n", file=file3, append=TRUE)
										}
									pila[[j]]$index <- pila[[j]]$index +1
									}
								}else{
								pila[[j]]$index <- pila[[j]]$index +1
								}
							}
						}else{ 
						pila[[j]]$index <- pila[[j]]$index +1
						}
					}
				
				}
			}
			j <- j-1
			if (j!=0)
				{
				pila[[j]]$index <- pila[[j]]$index +1
				print(j)
				}
		}
	}
	
}


