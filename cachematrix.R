## This file contains two functions which create a matrix and allow
## its inverse solution to be stored in the cache

## This function creates a list of functions allowing the matrix
## and its inverse to be set, retrieved, and for the inverse to be 
## stored in cache

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}

## This function checks for a cached inverse matrix solution for 
## the input matrix, and if a cached solution does not exist it 
## computes the inverse and stores it to cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  
  data <- x$get()
  
  inv <- solve(data)
  
  x$setinv(inv)
  
  inv
  
}
