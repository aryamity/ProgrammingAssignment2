## this function will create a matrix and cache the inverse of that matrix
##

## this function accepts matrics as input and returs parameters that will be used in the cache calculating its inverse 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}




## this function calculates the inverse of the matrics ; it checkes if the value exists in the cache , else it recalculates and save it in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- Inverse(data, ...)
  x$setInverse(m)
  m
  
  }
