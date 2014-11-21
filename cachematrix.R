## The functions compute the inverse of a matrix and stores the inverse in the cache
## A call of the function with no changes in the matrix will fetch the inverse of the matrix from the cache,
## thereby skipping the computation of the inverse. If the matrix has changed a computation will happen.

## The makeCacheMatrix function will create an R object which is a list of functions to manage the value in the cache

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function checks the cache for the presence of the inverse value. 
## If value exists it is returned else a new inverse is calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i
}
