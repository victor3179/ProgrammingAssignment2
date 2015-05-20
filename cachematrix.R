## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that returns a list of four methods (functions)
## that provide interface to inverted matrix cache.
## Functions are :

### set        -  saves original matrix
### get        -  returns original matrix
### setInverse -  saves inverted matrix in a cache
### getInverse -  returns inverted matrix from Cache



makeCacheMatrix <- function(x = matrix()) {
      inv_mtrx <- NULL
      set <- function(m) {
            mtrx <<- m
            inv_mtrx <<- NULL
      }
      get <- function() mtrx
      setInverse <- function(inverted) inv_mtrx <<- inverted
      getInverse <- function() inv_mtrx
      list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## cacheSolve is a function that looks up inverted matrix in a cache
## if inverted matrix is not found in the cache its value is computed,
## stored in a cache and returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      i <- x$getInverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      m <- x$get()
      i <- solve(m)
      x$setInverse(i)
      i
}
