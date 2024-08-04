## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Method to set the matrix 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Method to get the matrix 
  get <- function() x
  
  # Method to set the inverse of the matrix 
  setInverse <- function(inverse) inv <<- inverse
  
  # Method to get the inverse of the matrix 
  getInverse <- function() inv
  
  # Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  # If the inverse is alresdy calculated, we can retrieve it from the cache
  if  (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #Otherwise, we can calculate the inverse and set it in the cache
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  
  inv
}
  
  
  
  
 # Method set the matrix
  set <- function(y) {
    x <<- NULL
    inv <<- NULL
  }
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}

