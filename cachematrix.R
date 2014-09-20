## Programming Assignment 2
## This function creates a special "matrix" object that can cache its inverse.

## makeCacheMatrix function


makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL                                       ## Initialize the inverse property
  set <- function(y) {                                ##set the value of the matrix 
      x <<- y                                   
      inv_x <<- NULL                           
  }
  
  get <- function() x                                 ##get the value of the matrix 
  setinverse<- function(inverse) inv_x <<-inverse     ##set the value of the inverse
  getinverse <- function() inv_x                      ## get the value of the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv_x <- x$getinverse()                             ## Return a matrix that is the inverse of 'x'
  if (!is.null(inv_x)) {                              ## Just return the inverse if its already set
          message("getting cached inverse matrix")
          return(inv_x)
}
  data <- x$get()                                     ## Get the matrix from our object
 
x <- solve(data) %*% data                             ## Calculate the inverse using matrix multiplication
x$setInverse(x)                                       ## Set the inverse to the object
x                                                     ## Return the matrix

}
