## makeCacheMatrix will store and cache the matrix
## cacheSolve return the inverse of a matrix

## makeCacheMatrix create a list containg a function to
##      1. set the value of the vector
##      2. get the value of the vector
##      3. set the value of thte mean
##      4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve wil return the cached inverse of a matrix; the function
## compute to look for the inverse if it is not been cached

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
