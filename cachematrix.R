## These functions provide a caching matrix-inverting algorithm.
## There is a function for producing a specialised caching datastructure,
## and a function for inverting the matrix and caching the result, if required, or 
## retriving the cached result, if present.
##
## This assumes that the matrix passed in invertible.

## This function takes a matrix (parameter 'm') and returns a data structure
## that contains the input matrix and will cache the result of the inverted matrix 
## (see the `makeCacheMatrix` function below). If no matrix is provided, an empty 
## matrix is set by default.
##
## The format of the datastructure is a list with the following function fields:
## set <- takes a single argument and sets a new value for the matrix. The cache is
## invalidated when this is called.
## get <- returns the value of the matrix.
## setinverse <- sets the cached value.
## getinverse <- gets the cached value.

makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set <- function(y) {
    m <<- y
    i <<- NULL
  }
  get <- function() m
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function takes an instance of the datastructure provided by `makeCacheMatrix`
## (parameter 'x'). If the cache is not empty, it returns the stored result. Otherwise 
## it performs the matrix inversions, caches the result and then returns it.

cacheSolve <- function(x) {
  i <- x$getinverse()
  if(!is.null(i)) {
    return(i)
  }
  data <- x$get()
  result <- solve(data)
  x$setinverse(result)
  result
}
