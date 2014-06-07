## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This makeCacheMatrix function creates a
## special matrix object that can cache
## its own  matirx inversion, by taking
## advantage of the  <<- operator

makeCacheMatrix <- function(invertible = matrix()) {
  cachedinversion <- NULL
  set <- function(y) {
  invertible <<- y
  cachedinversion <<- NULL
  }
  get <- function() invertible
  setinversion <- function(inversion) cachedinversion <<- inversion
  getinversion <- function() cachedinversion
  list(set = set, get = get,
       setinversion = setinversion,
       getinversion = getinversion)  
}


## Write a short comment describing this function

## This cacheSolve function computes the inverse of
## the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(invertible, ...) {
## Return a matrix that is the inverse of 'x'
  inversion <- invertible$getinversion()
  if(!is.null(inversion)) {
    message("getting cached data")
    return(inversion)
  }
  data <- invertible$get()
  inversion <- solve(data, ...)
  invertible$setinversion(inversion)
  inversion  
}
