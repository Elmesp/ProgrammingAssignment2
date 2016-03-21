## The following functions functions that calculate and cache the inverse of a matrix.


## Creates and returns a list with attributes to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Calculates the inverse of the matrix x created with the above function.
## It first checks to see if the inverse has already been calculated,
## and calculates if hasn't or retrieves the value from the cache if
## it has been calculated

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}