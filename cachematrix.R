## R functions that are able to cache the potentially time-consuming computations of 
## inverting a matrix with the solve function (validity of input for solve function
## is assumed and not tested within function), based on given example in 
## Coursera R-Programming, Assignment 2. 
## Rationale: If the matrix is not changed, it may make sense to cache the value of the 
## invers so that when we need it again, it can be looked up in the cache and does not 
## have tobe recomputed.

## makeCacheMatrix initializes the value s that will take the inverse and creates a
## special vector that is basically a list of functions. 
## Call e.g. with x<-makeCacheMatrix(matrix(c(1.2,3.4,2.5,6.7,9.1,2.3,3.4,2.3,1.2),3,3))
## or any other invertible matrix.

## Remark: I do not see how "set" is  used here, could also be left out imo, if then 
## taken out of list as well.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
 set <- function(y) {
  x <<- y
  s <<- NULL
  }
  get <- function() x
  setinv <- function(solve) s <<- solve
  getinv <- function() s
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}

## Function calculates inverse of matrix (data given to above function) and 
## assigns result to s by calling above sub-function setinv. If this has already
## been done previously and data has not changed, function calls data from cache.
## Call e.g. with cacheSolve(x) after calling above function with above command.
## Call again to see that data is drawn from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinv()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinv(s)
  s
}