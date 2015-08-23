## Here is a set of functions that will calculate the inverse of a matrix.
## In case the inverse was already calculated it will be cached for further
## caluclations.
## 1. You should call the makeCacheMatrix function first with the matrix input.
## Store the result in variable.
## 2. Then you can trigger the cacheSolve function with the stored variable
## several times.
## When you trigger it for the first time it will calculate and cache the
## inverse of the stored variable. Triggering for the second time it will check
## the cache and retrieve the inverse matrix from there if that already exist.

## This function creates a special "vector" of list containing 4 functions.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This fuction will get the inverse of a stored matrix. In case it is cached
## it will get the value from the cache.
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
