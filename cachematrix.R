## Put comments here that give an overall description of what your
## functions do

## This function recieves creates a list with 4 functions that work with a matrix
## the set function recieves the matrix and stores it in the parent environment
## the get function retrieves the matrix from the parent environment
## the set inv allows the user to set the inverse of the matrix in the argument
## the get inv retrieves the matrix inverse, if there is none is returns a null

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function uses the function Solve to obtain the inverse of a square matrix
## if there is an inverse already stored, it runs the if cycle and returns that one
## if there is not an inverse stored, it calculates it and stored using the function
## set inv in makeCacheMatrix

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}