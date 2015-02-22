# Matrix inversion can be a costly computation. 
# Caching the inverse of a matrix rather than compute it repeatedly could be better solution.
# These two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list;function can do the following
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
   get <- function() return(x)
   setinverse <- function(inverse) inv <<- inverse
   getinverse <- function() inv
   list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# cacheSolve returns the inverse of the matrix. 
# First, it checks if the inverse has already been computed. 
# If so, it gets the result and skips the computation. 
# If not, it computes the inverse and cache the value through  setinverse function.

cacheSolve <- function(x, ...) {
   inv <- x$getinverse()
   if(!is.null(inv)) {
      message("Getting cached data...")
      return(inv)
   }
   data <- x$get()
   inv <- solve(data)
   x$setinverse(inv)
   inv
}


