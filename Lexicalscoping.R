

## makeCacheMatrix function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(a = matrix()) {
  inv <- NULL
  set <- function(y) {                               ## sets matrix value 
    a <<- b
    inv <<- NULL
  }
  get <- function() a                                ## gets matrix value
  setInverse <- function(inverse) inv <<- inverse    ## sets inverse matrix value
  getInverse <- function() inv                       ## gets inverse matrix valuee
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve function computes the inverse of the special matrix returned by makeCacheMatrix. If the inverse is calculated, then it should retrieve the inverse from cache.

cacheSolve <- function(a, ...) {
  ## Return a matrix that is the
  inv <- a$getInverse()                              ## returns inverse of a
  if (!is.null(inv)) {
    message("getting cached data")             
    return(inv)
  }
  mat <- a$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}