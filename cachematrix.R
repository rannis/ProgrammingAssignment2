## The following functions returns the inverse of a matix, caches the result,
## and reads the cached result without running the process intensive calculating for large data.

## makeCacheMatrix() function read and writes the input matrix and the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve() function checks if the inverse matrix is already cached, otherwise it calculates
## the inverse matrix and caches by calling setinverse() on makeCacheMatrix.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
