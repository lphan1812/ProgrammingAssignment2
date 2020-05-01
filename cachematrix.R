## Side note: for Mac OS, in order to install "matlib" to use function inv(), I had to 
## download XQuartz. 
## In this assignment, install.packages("matlib") and library(matlib) was run outside the script
## Explain the function:
## The makeCacheMatrix function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) m <<- inv
  getInverse <- function() m
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## The below function computes the inverse of the matrix created with the function above.
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the 
## cache via the setInverse function.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- inv(data,... )
  x$setInverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
}

## Example of using the functions created above:
## example <- makeCacheMatrix()
## example$set(matrix(1:4, 2))
## example$get()
##        [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## cacheSolve(example)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## cacheSolve(example)
## getting cached data
## [,1]     [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


