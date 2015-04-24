# Finding a matrix inverse may turn to be a costly computation, so it may be 
# beneficial to cache the inverse of a matrix rather than computing it over and over again
# This .R code contains a pair of functions that cache the inverse of a matrix.

## The function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  # List of variables/arguments specified in a function:
  #   "x" - the matrix object 
  #
  # Returns:
  #   A list containing functions that:
  #     1. set the value of the matrix
  #     2. get the value of the matrix
  #     3. set the value of the inverse
  #     4. get the value of the inverse
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    return(m <<- NULL)
  }
  
  get <- function() {
    return(x)
  }
  
  setmatrix <- function(solve) {
    return(m <<- solve)
  }
  
  getmatrix <- function() {
    return(m)
  }
  
  return(list(set=set,
              get=get,
              setmatrix=setmatrix,
              getmatrix=getmatrix))
}

# This function computes the inverse of that special "matrix" produced by makeCacheMatrix.
# If the inverse has already been calculated while the matrix has not been changed,
# cacheSolve than retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
 
  # List of variables/arguments specified in a function:
  #   x: the special "matrix" returned from makeCacheMatrix
  #
  # Returns:
  #   The inverse matrix
  
  m <- x$getmatrix()
  
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  } else {
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    return(m)
  }
}
