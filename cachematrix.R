## This assignment creates a cached copy of the inverse of a matrix

## The first function is "makeCacheMatrix" which takes in a matrix, x and
## saves a cached value of the inverse of matrix x

makeCacheMatrix <- function(x = matrix()) {
     # m is initiated to NULL value, but will hold the inverse of matrix x 
     m <- NULL
     
     # The set function looks through the parent environment for existing y to assign to x
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     
     # The get function retrieves the value of the matrix, x
     get <- function() x
     
     # The setsolve function calculates the inverse of the matrix
     # The inverse of x is assigned to matrix m in parent environment
     setsolve <- function(solve) m <<- solve
     
     # The getsolve retrieves the inverse of the matrix
     getsolve <- function() m
     
     list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}


## The cacheSolve function returns the inverse of a matrix by
## first checking to see if there's a cached copy of the matrix
## and if not, calculating the inverse of the matrix

cacheSolve <- function(x, ...) {
     
     ## m is assigned the result of x$getsolve()
     m <- x$getsolve()
     
     ## if a cached copy of m (the inverse of a matrix) exists, then it is returned
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     
     ## if no cached copy of m exists, then matrix x is retrieved
     data <- x$get()
     
     ## then the inverse of matrix x is calculated and assigned to m
     m <- solve(data, ...)
     x$setsolve(m)
     m
}
