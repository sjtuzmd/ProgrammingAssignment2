## Pair of functions that store a matrix and cache its inverse
## Re-sumbitting on 28 May 2019

## Function: makeCacheMatrix
## Description: This function creates a special "matrix" object 
## which contains a list of functions that set/get the matrix 
## and its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## "x" is the inversible matrix, while "solve" is the inverse 
  ## of the matrix
  solve <- NULL
  
  ## Function to set new matrix, and clear the cache of inverse
  set <- function(y) {
    x <<- y
    solve <<- NULL
  }

  ## Function to get the inversible matrix
  get <- function() x

  ## Function to set the inverse of the matrix
  setsolve <- function(s) solve <<- s

  ## Function to get the inverse of the matrix
  getsolve <- function() solve

  ## Return a list of the functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Function: cacheSolve
## Description: This function tries to get inverse of the matrix
## from cache. If cache doesn't contain the inverse, it will
## compute the cache for the matrix.

cacheSolve <- function(x, ...) {

  ## Try to get the inverse of the matrix
  s <- x$getsolve()

  ## If cache contains the inverse, it will return the inverse 
  ## directly
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }

  ## If not previously cached, it will get the inversible matrix
  ## in "data. And apply "solve" function to compute the inverse
  ## and store the inverse in "s".
  data <- x$get()
  s <- solve(data, ...)

  ## Set the inverse into cache
  x$setsolve(s)
  
  ## Return a matrix that is the inverse of 'x'
  s
}