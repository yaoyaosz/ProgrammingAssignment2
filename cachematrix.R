
## This function return a list contains four values, set,get,setinv,getinv.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {
    x
  }
  setinv <- function(newInv) {
    inv <<- newInv
  }
  getinv <- function() {
    inv
  }
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## this function checks if there is a inverse value of x in the cache. If so, returns the inverse. if not, calculates the inverser.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  calInv <- solve(data, ...)
  x$setinv(calInv)
  calInv
}


## Return a matrix that is the inverse of 'x'
