## Programming Assignment 2
## This function creates a special "matrix" object that can cache its inverse.

## makeCacheMatrix function


makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL                              
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

cacheSolve <- function(x, ...) {
  inv_x <- x$getinverse()
  if (!is.null(inv_x)) {
          message("getting cached inverse matrix")
          return(inv_x)
}
  inv_x <- solve(x$get())
  x$setinverse(inv_x)
  return(inv_x)

  
}
