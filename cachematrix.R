## These two functions make it so that expensive matrix inversion computations 
## don't need to be repeated unless the underlying matrix data has changed. This
## significantly speeds up execution for large matrices.
## Usage (example): 
##        >test <- matrix(runif(100000000,1,10), nrow=10000, ncol=10000)
##        >cachedM <- makeCacheMatrix(test)
##        >cacheSolve(cachedM)
##        >cacheSolve(cachedM)      ## a second invocation will access the cache

## This function leverages lexical scoping to surface an API for caching and 
## accessing the original matrix as well as the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function (inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function returns the pre-computed inverse matrix if it exists, otherwise
## it computes the inverse matrix and saves the result in the cache for future use

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}