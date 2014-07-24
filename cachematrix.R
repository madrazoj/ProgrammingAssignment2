## The makeCacheMatrix and cacheSolve work together to provide a wrapper for a matrix object
## as well as caching of an inverted version of the wrapped matrix that is executed on demand
## only once. Subsequent calls leverage the cached version.

## makeCacheMatrix is a function that returns a list of functions available for a wrapped matrix object.
## makeCacheMatrix provides an object like wrapper to a matrix, which is provided as a parameter 
## to the function. The matrix operations work when called from sub-routines or from the global 
## environment.
## makeCacheMatrix provides the following methods:
##  get() -- returns the wrapped matrix.
##  set() -- override the matrix originally used in the creation of the function list. 
##      The matrix inverse is also overridden to NULL.
##  setInverse() -- sets the value of the inverse matrix as determined by the caller. The caller
##      could set an invalid value. The accuracy is not checked. This allows the inverted value to be cached.
##  doInverse() -- calculates then returns the inverted matrix of the wrapped matrix. This is preferable
##      to having different clients figure how to do the matrix inversion operation.
##  getInverse() -- returns the matrix as set by the setInverse() function.
makeCacheMatrix <- function(x = matrix()) {
  m <- x
  mi <- NULL
  set <- function(y) {
    m <<- y
    mi <<- NULL  
  }
  get <- function() m
  doInverse <- function() {
    solve(m)
  }
  setInverse <- function(inverse) mi <<- inverse
  getInverse <- function()  mi
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse,
       doInverse = doInverse)  
}

## cacheSolve takes a list of functions as defined by makeCacheMatrix and uses it 
## to return an inversion of the matrix wrapped by makeCachMatrix.
## It uses makeCacheMatrix to generate the inverted matrix if the inverted matrix has not
## been calculated. Otherwise, it returns the cached value held by makeCacheMatrix instance
## contained in the x parameter.
cacheSolve <- function(x, ...) {  
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- x$doInverse()
  x$setInverse(m)
  m  
}
