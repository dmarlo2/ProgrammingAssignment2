## Matrix inversion can involve costly computations that may be avoided through lexical scoping. 
## The functions below are a means of doing so.

## The makeCacheMatrix function creates a special, cached matrix which is a really a list
## containing a function to set the value matrix, get the value of the matrix, set the value of
## the inverse, and then get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x<<- y
    inv <<- NULL
  }
  Get <- function() x
  SetInv <-function(inverse) inv<<- inverse
  GetInv <-function() inv
  list(set = set, 
       Get = Get, 
       SetInv = SetInv, 
       GetInv = GetInv)
}


## The second function (cacheSolve), calculates and returns the matrix of the special matrix
## that was created by the makeCacheMatrix function.  If the inverse has already been calculated
## then it gets the inverse from the cache and skips the costly computation.

cacheSolve <- function(x, ...) {
  inv <- x$GetInv()
  if(!is.null(inv)) {
    message("Receiving Cached Matrix")
    return(inv)
  }
  data <- x$Get()
  inv <- solve(data, ...)
  x$SetInv(inv)
  inv
}
