## This functions allow to compute inverse of matrix 
## and cache computed value to save time on recalculation 

######
## Create a special "matrix" object 
## that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL

  # Set initial matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }

  # Get initial matrix
  get <- function() {
    x
  }
  
  # Set inverse
  setinverse <- function(y) {
    inverse <<- y
  }
  
  # Get inverse
  getinverse <- function() {
    inverse
  }
  
  # Return matrix object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

######
## Calculate inverse of matrix 
## or get it from the cache if it's already calculated
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()

  if(!is.null(inverse)) {
    # Inverse is already calculated, return value from cache
    message("getting cached data")
    return(inverse)
  }
  
  # Inverse is not calculated. Get matrix data, calculate and cache the result
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  
  ## Return calculated inverse
  return(inverse)
}
