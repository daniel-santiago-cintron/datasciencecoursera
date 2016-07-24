## The functions below SHOULD, create a matrix object
## that could be called upon to calculate the inverse

## First the function, modeled after the example, should create a matrix
## then it should solve it(inverse). Once the m value is set in any other enviroment
## it could, in theory, check for a stored solution. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    inversed <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         inversed = inversed,
         getinverse = getinverse)
  }
  
  
## In this section, the function is designed to catch the inverse of the matrix
## cached by the previous function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$inversed(m)
  m  
}
  
