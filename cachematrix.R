##  Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setmatrix <- function(y) {
    x <<- y
    i <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse, getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  matdata <- x$getmatrix()
  i <- solve(matdata, ...)
  x$setinverse(i)
  i
}
