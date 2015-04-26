#function makeCacheMatrix is meant to create a matrix object and is capable of caching the inverse of the matrix
#function cacheSolve is meant to calculate the inverse of the matrix. If the inverse has already been calculated and cached, 
#   it will take the inverse from the cache

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  } #function sets value of matrix
  get <- function() x #function gets value of matrix
  setinversematrix <- function(inverse) a <<- inverse #function sets value of the inverse matrix
  getinversematrix <- function() a #function gets the value of the inverse matrix
  list(
    set = set,
    get = get,
    setinversematrix = setinversematrix,
    getinversematrix = getinversematrix
  )
}

# As described above, cacheSolve function will calulate inverse matrix unless it has already been calculated, cached, and is unchanged.
#   If this case, it will return the cached, inverted matrix.
#   Otherwise, it will set the value of the inverse matrix using setinversematrix function.

cacheSolve <- function(x, ...) {
  a <- x$getinversematrix()
  if(!is.null(a)) {
    message("Getting cached data...")
    return(a)
  }
  data <- x$get()
  a <- solve(data, ...)
  x$setinversematrix(a)
  a
}