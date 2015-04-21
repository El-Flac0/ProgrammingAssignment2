## These functions create a matrix object that caches it's inverse. In order to save
## time, if the inverse has already been calculated and the matrix is unchanged,
## the second function will retrieve the matrix from the cache rather than compute it.

## makeCacheMatrix creates a 'matrix' object that caches it's inverse for later retrieval.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinv <- function(solve) i <<- solve
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve checks to see if the inverse has been cached, and solves the matrix if not.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if (!is.null(i)) {
      message("retrieving cached data...")
      return(i)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$setinv(i)
    i
}
