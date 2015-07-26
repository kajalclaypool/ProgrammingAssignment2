## makeCacheMatrix: creates a special "matrix" object that can cache its inverse
## returns a list containing a set of functions
## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the cached inverse of the matrix
## 

makeCacheMatrix <- function(m = matrix()) {
  inverse_m <- NULL
  set <- function(y) {
    m <<- y
    inverse_m <<- NULL
  }
  get <- function() m
  setinverse <- function(y) inverse_m <<- y
  getinverse <- function() inverse_m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve retrieves the inverse from the cache.
## inverse_m = inverse matrix
cacheSolve <- function(x, ...) {
    inverse_m <- x$getinverse()
    if(!is.null(inverse_m)) {
      message("getting cached data")
      return(inverse_m)
    }
    md <- x$get()
    inverse_m <- solve(md)
    x$setinverse(inverse_m)
    ## Return a matrix that is the inverse of 'x'
    inverse_m
}

## testFunction to enable testing the cached inverse function
## 
testFunction <- function () {
  ## create a test matrix
  m <- matrix (c(2,3,3,4), ncol=2, nrow=2)
  
  ## create the CacheMatrix
  x_matrix <- makeCacheMatrix(m)
  
  ## call cacheSolve: should compute the inverse
  inverse_first <- cacheSolve(x_matrix)
  print(inverse_first)
  
  ## call cacheSolve again: should return the cached value
  cacheSolve(x_matrix)
}