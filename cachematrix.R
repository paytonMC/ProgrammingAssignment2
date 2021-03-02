## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  matrix_inverse <- NULL
  set <- function(y) {
    x <<- y
    matrix_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inversevalue) matrix_inverse <- inversevalue
  getinverse <- function() matrix_inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  matrix_inverse <- x$getinverse()
  if(!is.null(matrix_inverse)) {
    message("getting cached data")
    return(matrix_inverse)
  }
  matrix <- x$get()
  matrix_inverse <- solve(matrix)
  x$setinverse(matrix_inverse)
  matrix_inverse

}
