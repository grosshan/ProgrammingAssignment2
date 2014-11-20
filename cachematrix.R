## This file contains 2 functions for special matrices, that can 
## cache their inverses:
##    makeCacheMatrix(x)  <- constructor a special matrix 
##    cacheSolve(x)       <- returns the inverse of a "special" matrix


## Creates an object that contains a matrix, the inverse and 4 additional
## functions
##    get()       - return the contained matrix object
##    set(y)      - set the contained matrix data to y
##    getsolve(y) - return the inverse matrix object - or 
##                  returns NULL if no inverse was set yet
##    setsolve(y) - set the inverse matrix data to y

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y){
    x <<- y;
    x_inv <<- NULL
  }
  get <- function(){x}
  setsolve <- function(y){x_inv <<- y}
  getsolve <- function(){x_inv}
  list(set = set, get = get, setsolve = setsolve,
       getsolve = getsolve)
}


## This function returns the inverse of a matrix. If the inverse was arleady computed
## the function returns the cached result. Otherwise, the inverse will be computed 
## and the result will be cached for future use.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x_inv <- x$getsolve();
  if(!is.null(x_inv) ){
    message("getting cached data")
    return(x_inv)
  }
  
  data <- x$get();
  x_inv <- solve(data);
  x$setsolve(x_inv);
  x_inv
}
