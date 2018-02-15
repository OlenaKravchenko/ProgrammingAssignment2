## These functions calculate the inverse of the matrix and store it in the 
## cache. Once the matrix changes, they re-cache the correct updated inverse. 

## The first function creates a matrix and stores the cached value of the inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
  inver = NULL
  set = function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
## Here the function creates the inverse of the matrix.
  set_inv <- function(inverse) inver <<- inverse
  get_inv <- function() inver
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv =  get_inv)

}


## The second function returns the cached value of the matrix if it has 
##been calculated at least once, else it calculates teh inverse of the matrix
##and saves it to the cache. 

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
