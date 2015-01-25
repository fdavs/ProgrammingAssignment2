## This set of functions creates memoization-capable "smart" matrices: "smart" 
## in the sense that they remember the result of expensive matrix operations so 
## that these results can be reused again rather than calculated again every time
## they are needed.
##
## Note. This is a limited implementation that focuses on the calculation of 
## the inverse of matrices. 

## Generates a smart matrix object that is able to cache the result of 
## matrix operations.
## 
## @param m The matrix that is to be made smart (able to remember its inverse)
##          By default, an empty matrix is used.

makeCacheMatrix <- function(m = matrix()) {
  inverse <- NULL
  set <- function(x) {
    m <<- x
    inverse <<- NULL
  }
  get <- function() m
  setinverse <- function(x) inverse <<- x
  getinverse <- function() inverse
  list(
    set = set, 
    get = get, 
    setinverse = setinverse,
    getinverse = getinverse)
}

## Returns the inverse of the given matrix. For performance, if the
## inverse has already been calculated and the matrix has not been changed,
## a cached result is returned rather than calculating the inverse again.
##
## @param m The matrix whose inverse is to be calculated.
##          This must be an object previously created by makeCacheMatrix()

cacheSolve <- function(m, ...) {
  inverse <- m$getinverse()
  if (is.null(inverse)) {
    message("calculating inverse")
    inverse <- solve(m$get())
    m$setinverse(inverse)
  }
  inverse
}
