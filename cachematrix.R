## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Function makeCacheMatrix creates a special "vector", which is really a list containing a function to
## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

## The following function calculates the inverse matrix
## of the special "vector" created with the above function.
## However, it first checks to see if the inverse matrix has
## already been calculated. If so, it gets the inverse matrix
## from the cache and skips the computation. Otherwise, it
## calculates the inverse matrix of the data and sets the value
## of the inverse matrix in the cache via the setmean function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
