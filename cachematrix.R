## Put comments here that give an overall description of what your
## functions do
## 
## these functions create a caching mechanism for solving a inverse matrix.
## Using makeCacheMatrix we create an object that is a list of functions and has 
## a cache variable inside of it.
## Using cacheSolve and passing the object created with makeCacheMatrix will
## calculate the inverse matrix and store it in the cache. Every next call to 
## cacheSolve will return the inverse matrix stored in cache.


## Write a short comment describing this function

## makeCacheMatrix creates a list with four functions that enable you to
## to cache the inverse matrix after using the function solve on a the original 
## matrix.
## These functions are:
## set: set the original matrix
## get: get the original matrix
## setsolve: set the solved inverse matrix
## setsolve: get the solved inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
  s<- NULL
  set <- function(y) {
    x<<- y
    s<<- NULL
  }
  get <- function() x
  setsolve <- function(solved) s<<- solved
  getsolve <- function() s
  list(set = set, 
       get = get, 
       getsolve = getsolve, 
       setsolve = setsolve)
}


## Write a short comment describing this function

## The cacheSolve function returns the inverse matrix of a matrix, calculated 
## with the solve function. The argument passed to the function is an object (x)
## created with the makeCacheMatrix function.
## If the cacheSolve function has solved the matrix it will store the result in
## object x. That way it can reuse the result in future calls.

cacheSolve <- function(x, ...) {
  # get the cached data
  s <- x$getsolve()
  
  if (is.null(s)) {
    # there is no cached data, so we need to calculate the answer first
    s <- solve(x$get()) 
    # store the answer in the cache
    x$setsolve(s)
  }
  else {
    # notify user we are returning cached data
    message("getting cached data")
  }
  
  return(s)
}
