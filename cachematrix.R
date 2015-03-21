## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    #determine if x is square
    if(nrow(x) != ncol(x)) stop(error="it is only possible to find the inverse of a square matrix")
    
    #set initial values to null
    inv <- NULL
    
    #function to set
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    #function to get (value of x)
    get <- function() x
    
    #function to set inverse
    setinv <- function(tempinv) inv <<- tempinv
    
    #function to get inverse
    getinv <- function() inv
    
    #return a list of functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    #check if data is in cache
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    #function hasn't returned, so data wasn't in cache, hence we should put it there
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    
    #now return the inverse
    inv
}
