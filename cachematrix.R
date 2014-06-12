## This pair of functions work in conjunction to
## allow saving computating time by caching the
## the inversion of the matrix if it has been
## calculated already. This is by taking advantage
## of the scoping rules of the R language and how 
## they can be manipulated to preserve state inside
## of an R object. This is in turn achieved by using
## the  <<- operator, which can be used to assign a
## value to an object in an environment that is
## different from the current environment.

## This makeCacheMatrix function creates a
## special matrix object that can cache
## its own  matirx inversion. It contains the
## functions to set/get the value of the matrix
## and to set/get the value of the inverted matrix.

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