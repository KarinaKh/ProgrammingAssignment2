## makeCacheMatrix and cacheSolve functions cache the inverse matrix to avoid repeating
## the calculation of invese of the matrix. 

## function makeCacheMatrix creates a special Matrix which is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) i <<- inverse
  get_inverse <- function() i
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## function cacheSolve fist checks if the inverse of the matrix has already been calculated, if it has been it gets the 
## result and skips the calculation. If not, it calculates the inverce and sets the result in the cache

cacheSolve <- function(x, ...) {
  i <- x$get_inverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$set_inverse(i)
  i
}
