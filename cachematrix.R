## Put comments here that give an overall description of what your
## functions do
##lexical scoping, caching inverse of a matrix

## Write a short comment describing this function
##This function creates matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  matrix_inv <- NULL
  set <- function(y) {
    x <<- y
    matrix_inv <<- NULL
  }
  get <- function() x
  
  setInverse <- function(solveMatrix) matrix_inv <<- solveMatrix
  getInverse <- function() matrix_inv
  list(set = set, get = get, setInverse = setInverse, getInverse= getInverse)
}


## Write a short comment describing this function
##This function either computes matrix created from above or extracts computation from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrix_inv <- x$getInverse()
  if(!is.null(matrix_inv)){
    message("getting cached data")
    return(matrix_inv)
  }
  data <- x$get()
  matrix_inv <- solve(data)
  x$setInverse(matrix_inv)
  matrix_inv
}
