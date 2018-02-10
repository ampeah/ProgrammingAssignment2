## Caching the inverse of a Matrix:
## The Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The pair of functions below are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set <- function(y) {
    x <<- y
    inv<<- NULL
  }
  get <- function() x
  Setinverse <- function(inverse) inv <<- inverse
  Getinverse <- function() inv
  list(set = set, get = get,
       Setinverse = Setinverse,
       Getinverse= Getinverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cachesolve <- function(x, ...) {
  inv <- x$Getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- inverse(mat, ...)
  x$Setinverse(inv)
  inv
}