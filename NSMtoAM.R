#### Function to convert Nested Sets Model of renumbered datasets into Adjacency Matrix
#### Version 1.0
#### 2016-04-22

#########################################################################
# Nested Sets Model to Adjacency Matrix                                 #
#########################################################################
NSMtoAM <- function(NSM){
  # Creating empty matrix for adjacency matrix 
  n <- length(NSM$OBJECTID) # Creating empty matrix for adjacency matrix 
  M <- matrix(0,n,n) #can become very large matrix
  pb <- txtProgressBar(min = 0, max = 1, initial = 0, char = "=")
  
  # Actual calculation of matrix values
  for (i in c(1:n)){
    up <- which(NSM$H1[i]+1==NSM$H1)
    ifelse(length(up)!=0, M[i,up] <- 1, M[i,up] <- 0)
    
    if (length(up)!=0 && NSM$H2[up]+1<NSM$H2[i]){
      up <- which(NSM$H2[up]+1==NSM$H1)
      ifelse(up!=0, M[i,up] <- 1, M[i,up] <- 0)
    }
    
    down <- which(NSM$H2[i]-1==NSM$H2)
    ifelse(down!=0, M[i,down] <- 1, M[i,down] <- 0)
    setTxtProgressBar(pb, i)
  }
  close(pb)
  return(M)
}