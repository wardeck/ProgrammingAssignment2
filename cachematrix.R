## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
    
    inverse = NULL
    setmatrix = function(y) {
      x       <<- y
      inverse <<- NULL
    }
    getmatrix = function() x
    setinverse = function(inverse) inverse <<- inverse 
    getinverse = function() inverse
    list(setmatrix=setmatrix, 
         getmatrix=getmatrix, 
         setinverse=setinverse, 
         getinverse=getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse = x$getinverse()
  
  if (!is.null(inverse)){
    message("cached data")
    return(inverse)
  }
  
  data = x$getmatrix()
  inverse = solve(data, ...)
  
  x$setinverse(inverse)
  
  return(inverse)
  
}
