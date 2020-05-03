## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # Set function: creates the local matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Returns the local matirx
  get <- function() x
  
  # Caches the inverse matrix
  setinv <- function(invm) inv <<- invm
  
  # retunr the inverse matrix
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # try to get the inverse matrix from cache
  inv <- x$getinv()
  # check if inverse matrix has been saved (in case retun cached and exit)
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # if not cached, calculate and cache
  tmpm <- x$get()
  tmpinv <- solve(tmpm)
  x$setinv(tmpinv)
  tmpinv
}
