## Programming Assignment 2.
## Caching the Calcualter Inverse of a Matrix. 
## The functions will calculate Matrix Inversion, which is a highly computational process
## The functions will cache the inverse of a matrix, so computational processing is saved when calculating the inverse due to using the cache.

## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
              x <<- y
              inv <<- NULL
      }
      get <- function() x
      setInv <- function(inverse) inv <<- inverse
      getInv <- function() inv
      list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by the makeCacheMatrix.
## If the inverse has already been calculated, then the cachesolve should retreive the inverse from the cache.

cacheSolve <- function(x, ...) {
       inv <- x$getInv()
       if(!is.null(inv)) {
         message("getting cached inverse")
         return(inv)
       }
       data <- x$get()
       inv <- solve(data,...)
       x$setInv(inv)
       inv
}

