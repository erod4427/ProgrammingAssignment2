## Put comments here that give an overall description of what your
## functions do

## Writ e a  short  comment describing this function
## this function defines 4 sub-functions, and
## returns each of them in the list

makeCacheMatrix <-  function(x = numeric()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) i <<-  inverse   
  getInverse <- function() i
  
  list(set = set, get = get,
       setInvr = setInverse,
       getInvr = getInverse)
}


## Write a short comment describing this function
## cacheSolve returns an inverse of a matrix
## it first checks to see if the inverse was previously calcuated
## by retrieving it from a cache, if it was previously calcuated it simply
## returns it, other wise it must : calculate it, put in the cache, and return it.

cacheSolve <- function(x, ...) {
  
  inverse <- x$getInvr()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInvr(inverse)
  inverse
}
