## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  set <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invMat <<- inverse
  getInverse <- function() invMat
  list(set=set, 
       get=get, 
       setInverse=setInverse, 
       getInverse=getInverse)
}


## This function computes the inverse of the special matrix returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve will retrieve 
## the inverse from the cache. 
## It is assumed that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  invMat <- x$getInverse()
  
  if(!is.null(invMat)) {
    message("getting cached data")
    return(invMat)
  }
  
  data <- x$get()
  invMat <- solve(data, ...)
  x$setInverse(invMat)
  invMat
}
