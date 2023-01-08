## Assignment2 Code

## This makeCacheMatrix function is to create a matrix that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This cacheSolve function is to compute the inverse of the maxtrix created by the makeCacheMatrix fucntion.
## If the inverse has already been calculated,the cacheSolve will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mdata <- x$get()
  i <- solve(mdata, ...)
  x$setInverse(i)
  i
}
