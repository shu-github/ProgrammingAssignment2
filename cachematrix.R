## This is for Programming in R assignment #2
## The assignement is to write a pair of functions that cache the 
## inverse of a matrix which could be computational costly

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inver) {
    inv <<- inver
  }
  getInv <- function() inv
  list( set = set, get = get, setInv = setInv, getInv = getInv)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## check to see if the Inverse of x has been computed
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting inverse matrix from cached data")
    return(inv)
  }
  matrix_data <- x$get()
  inv <- solve(matrix_data, ...)
  x$setInv(inv)
  inv
}
