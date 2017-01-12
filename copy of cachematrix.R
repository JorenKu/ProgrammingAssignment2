# General description
## makeCacheMatrix and cacheSolve are functions that create the inverse of a
## (reversible) matrix and keeps it in memory/cache so that the next time
## the same input is given, the inverse matrix doesn't have to be recalculated,
## which can be costly in time, but can be returned immediately.

# Description makeCachematrix
## makeCachematrix is a helper function in which 'setters' and 'getters' are defined:
## with new input it makes sure that an old inverse matrix is removed
## then it defines the functions necessary for solving the matrix
## by defining/getting from the input (x) to the inverse matrix (s) using solve
## finally it creates a list of those functions so that they can be used
## by cachesolve.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  
  get <- function() x
  
  setSolve <- function(solve) s <<- solve
  
  getSolve <- function() s
  
  list(set = set, get = get, 
       setSolve = setSolve,
       getSolve = getSolve)
  
}

# Description cacheSolve
## cacheSolve makes use of the functions created above first it checks
## if the input is the same as from memory either it gives back the 
## inverse immediately or it enters the new data in the setter/getter
## functions, so that the inverse is calculated and the inverse is put 
## in memory for the next time the inverse function is required

cacheSolve <- function(x, ...) {
  
  s <- x$getSolve()
  
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  
  data <- x$get()
  
  s <- solve(data, ...)
  
  x$setSolve(s)
  
  s
}

# Example 1
## A = matrix(c(2, 4, 3, 1), nrow=2, ncol=2, byrow = TRUE)        
## B <- makeCacheMatrix(A)
## cacheSolve(B)

# Output 1
##      [,1] [,2]
## [1,] -0.1  0.4
## [2,]  0.3 -0.2

## cacheSolve(B)
## getting cached data
##      [,1] [,2]
## [1,] -0.1  0.4
## [2,]  0.3 -0.2

# Example 2
## C = matrix(c(0, 2, 4, 0), nrow=2, ncol=2, byrow = TRUE)        
## D <- makeCacheMatrix(C)
## cacheSolve(D)

# Output 2
##      [,1] [,2]
## [1,]  0.0 0.25
## [2,]  0.5 0.00