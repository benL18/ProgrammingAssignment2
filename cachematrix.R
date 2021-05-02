## Put comments here that give an overall description of what your
## functions do

## this function defines a list of functions used to 
# 1. get the matrix, 
# 2. set the matrix, 
# 3. get the inverted matrix x_inv and 
# 4. set the inverted matrix  x_inv

makeCacheMatrix <- function(x = matrix()) {
x_inv <- NULL

set <- function(y) {
     x <<- y
     x_inv <<- NULL
}
get <- function() x
setinv <- function(inv) x_inv <<- inv
getinv <- function() x_inv
list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)
}


## the cacheSolve function is used on a matrix x already created via makeCacheMatrix
# when existing the inverted matrix x_inv is directly returned
# otherwise the data is extracted from x and the inverted matrix x_in is computed via solve(x)
# the inverted matrix is then cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     x_inv <- x$getinv()
     if(!is.null(x_inv)) {
          message("getting cached data")
          return(x_inv)
     }
     data <- x$get()
     x_inv <- solve(data, ...)
     x$setinv(x_inv)
     x_inv
}
