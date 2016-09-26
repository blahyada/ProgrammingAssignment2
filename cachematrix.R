## Uses the <<- operator to assign a value to an object in an environment that 
## is different from the current environment.

## Creates a sepcial "matrix", which is really a list containing a function to 
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of the inverse
##   4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated.
## If so it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the
## inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
