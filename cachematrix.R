## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##Create cache matrix
makeMatrix <- function(x = matrix()) {
  invM <- NULL
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invM <<- inverse
  getInverse <- function() invM
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
##invert matrix
cacheInvert <- function(x, ...) {
  invM <- x$getinverse()
  if(!is.null(invM)) {       ##look for cache data
    message("getting cached data")
    return(invM)
  }
  data <- x$get()
  invM <- solve(data, ...)
  x$setInverse(invM)
  invM
}
