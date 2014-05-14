# Create a list that contains a matrix and a cache for store the matrix's inverse.
makeCacheMatrix <- function(x = matrix()) {
  # Initialize inverse matrix.
  inv <- NULL
  
  # Get and set functions for the original matrix.
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function() x
  
  # Get and set functions for the inverse matrix.
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  # Create and return a list with the four functions.
  list(set = setMatrix, 
       get = getMatrix,
       setInv = setInverse,
       getInv = getInverse)
}

# Get the matrix's inverse of a matrix included in a list generated with the method 'makeCacheMatrix()'.
cacheSolve <- function(x, ...) {
  # Verify if the inverse matrix was already been calculated.
  inverse <- x$getInv()
  if(!is.null(inverse)) {
    # The inverse maxtrix is in the cache, return it.
    message("getting cached data")
    return(inverse)
  }
  
  # The cache is empty, calculate the inverse matrix.
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  
  # Save the inverse matrix into the cache and return it.
  x$setInv(inverse)
  inverse
}

# Example of use:
# source("cachematrix.R")
# m <- matrix(c(1,2,3,4), 2, 2)
# mbis <- makeCacheMatrix(m)
# cacheSolve(mbis)