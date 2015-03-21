matrixMultiply <- function(x = matrix(), y=matrix()){
    if (ncol(x) != nrow(y)) stop(error="Cannot multiply these two matrices. For matrix multiplication,  the number of columns in the first matrix must equal the number of rows in the second")
    
    res <- matrix(nrow=nrow(x), ncol=ncol(y))
    for(j in 1:ncol(res)){
        for (i in 1:nrow(res)){
            res[i,j] = sum(x[i,] * y[,j])
        }
    }
    return(res)
}

checkInverse <- function(x = matrix(), inv = matrix()){
    #first check if it's a square matrix
    if(nrow(x) != ncol(x)) return (FALSE)
    
    #if they are equal, then matrix multiplication should give the identity matrix
    prod <- matrixMultiply(x, inv)
    
    #verify that prod is the identity matrix
    for(j in 1:ncol(prod)){
        for (i in 1:nrow(prod)){
            if(i == j){
                #should be one
                if (prod[i,j] != 1) return (FALSE)
            }
            else{
                #should be zero
                if (prod[i,j] != 0) return (FALSE)
            }
        }
    }
    
    #got to the end with no problems, return true
    return (TRUE)
}