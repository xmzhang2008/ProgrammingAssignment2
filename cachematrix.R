## Put comments here that give an overall description of what your
## functions do

## Create a matrix object and cache its inverse

makeCacheMatrix <- function( m = matrix() ) {
  
  ## Assign NULL value to the initial matrix
  inv <- NULL
  
  ## Set the matrix
  set <- function( mx ) {
    m <<- mx         # assign mx value to m in the local environment
    inv <<- NULL
  }
  
  ##  get the matrix function
  get <- function() {
        m  ## Return the matrix
  }
  
  ## Assign the inverse of the matrix value function
  setInv <- function(invmx) {
    inv <<- invmx
  }
  
  ## Use the function to get the inverse of the matrix
  getInv <- function() {
       inv
  }

}




## Compute the inverse of the special matrix returned by the previous function "makeCacheMatrix"
##  If the inverse exists, then retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
   m <- x$getInv()
  
  ## Reture m if it has a value
  if( !is.null(m) ) {return(m)}
  
  
   OrigMx <- x$get()
  
  m <- solve(OrigMx) %*% OrigMx  # Calculate inverse
  
  ## Set the inverse to the object
  x$setInv(m)
  
  ## Return the matrix
  m
}
