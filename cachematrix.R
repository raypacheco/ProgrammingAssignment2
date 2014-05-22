## These pair of functions cachce the inverse of a matrix
## in order to save the costly computation of repeatedly
## calculating the inverse.

## The first function creates a matrix object which
## can cache the inverse. It ouputs a list of four functions:
## get, set, getinv, and setinv.

makeCacheMatrix <- function(x = matrix()) {
  ## if an object is called without a method
  i <- NULL
  
  ## sets the matrix in the matrix object and initializes
  ## the inverse as NULL.
  set <- function(y) {
          x <<- y
          i <<- NULL          
  }
  
  ## gets the matrix x that been set.
  get <- function() x
  
  ## saves the inverse cache.
  setinv <- function(inverse) i <<- inverse
  
  ## retrieves the inverse cache.
  getinv <- function() i
  
  ## returns the list of the functions.
  list(set = set, get = get, setinv = setinv,
       getinv = getinv)
}

## This second function computes the inverse of the matrix
## object returned from the above function. If the inverse
## has already been calculated and no changes have been made
## then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## retrieves the inverse of x
  ## if the inverse has been calculated then it retrieves the
  ## cached data and prints the message 'getting cached data'
  i <- x$getinv()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  ## if the inverse is not cached then it computes the inverse
  ## and caches the inverse to the setinv function.
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
