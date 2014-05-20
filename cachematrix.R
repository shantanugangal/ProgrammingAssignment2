## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special matrix object
## makeCacheMatrix returns a list containing functions to 
  # - set the value of the input Matrix
  # - get the value of the input Matrix
  # - set the value of the inverse to the input Matrix
  # - get the value of the inverse to the input Matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve ##
  getInverse <- function() m
  
  ## return the list with the functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## cacheSolve returns the inverse of the matrix in the makeCacheMatrix object
## it saves computation if the inverse has been computed once

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    ## SAVES THE RECOMPUTATION OF THE INVERSE / SOLVE OPERATION and returns the saved / precomputed inverse
    return(m)
  }
  ## if no inverse is saved, it calculates the inverse the first time
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
