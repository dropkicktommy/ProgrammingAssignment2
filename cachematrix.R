## These functions solve and cache inverse matrices.
## Usage:
##
## > a <- makeCacheMatrix()
## > a$set(matrix(c(1, 2, 3, 0, 1, 4, 5, 6, 0), nrow=3, ncol=3))
## > cacheSolve(a)
##
## note: matrix must be invertable


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # setup variables
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # create functions for getting, setting, etc. matrice data
  get <- function() x
  setmatrix <- function(solve) i <<- solve
  getmatrix <- function() i
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
## If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` 
## should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getmatrix()
  # check to see if this data is already cached
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # if data is not already cached solve for the inverse matrix
  data <- x$get()
  i <- solve(data, ...)
  # cache the result
  x$setmatrix(i)
  # return the result
  i
}
