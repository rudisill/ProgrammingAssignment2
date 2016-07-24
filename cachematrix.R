## These functions create a matrix, creates its inverse then caches the inverted matrix
## Coursera R programming- Assignment #2
## Student: P Rudisill

## This function creates and inverts a matrix called matrx

makeCacheMatrix <- function(x = matrix()) {
  
  matrx <- NULL
  
  # set the value of our matrix
  set <- function(y) {
  x <<- y
  matrx <<- NULL
  }
  
  # Get the value of our matrix
  get <- function() x
  
  # function to invert the matrix
  setInverse <- function(inverse) matrx <<- inverse
  
  # get the value of the inverted matrix
  getInverse <- function() matrx
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function to cache our matrix

cacheSolve <- function(x, ...) {
  ## call function above to get the inverted matrix
  invert <- x$getInverse()
  if (!is.null(invert)) {
    return(invert)
  }
  mat <- x$get()
  invert <- solve(mat, ...)
  # call function above to set the inverted matrix
  x$setInverse(invert)
  invert
}
