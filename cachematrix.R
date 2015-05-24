## Cache the inverse of a matrix

## This function creates a special "matrix" object 
## that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    set_inverse <- function(inv) inverse <<- inv
    get_inverse <- function() inverse
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above 
## If the inverse has already been calculated 
## (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$get_inverse()
    ## Check if inverse matrix was already calculated 
    ## and retrieve the content from the cache
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    ## Else calculate the inverse matrix
    data <- x$get()
    inverse <- solve(data, ...)
    x$set_inverse(inverse)
    inverse
}
