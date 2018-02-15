makeCacheMatrix <- function(x = matrix()) {
  inver = NULL
  set = function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  set_inv <- function(inverse) inver <<- inverse
  get_inv <- function() inver
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv =  get_inv)

}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver = x$get_inv()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
    matrix.data <- x$get()
    inver = solve(matrix.data,...)
    x$set_inv(inver)
    return(inver)
}
