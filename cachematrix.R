## These function cache the inverse of a matrix

## Create a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inverse <<- solve
  getinv <- function() inv
  list(get=get, set=set, getinv=getinv, setinv=setinv)
}

## Computes the inverse of the special
##"matrix" returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x[getinv()]
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x[get()]
  inv <- solve(data)
  x[setinv(inv)]
        ## Return a matrix that is the inverse of 'x'
  inv
}
