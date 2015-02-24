concat <- function(a)
{
    a <- as.matrix(a)
    dima <- dim(a)
    vect <- as.vector(matrix(NA,dima[1], 1))
    for (i in 1:dima[1])
    {
          past<-a[i,1]
          j<-2
          while(j <= dima[2])
          {
          past<-paste(past, a[i,j], sep="_")
          j<-j+1
          }
    vect[i]<-past
    }

vect
}

