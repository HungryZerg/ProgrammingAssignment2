##  Two functions that are used to create a special object
##  that stores a matrix and cache's its inverse

## makeCacheMatrix() creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # i - inverse matrix value
    i <- NULL
    
    # set function: set value of the matrix and reset inverse matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # get function: get value of the matrix
    get <- function() x
    
    # setInverse function: set value of the inverse matrix
    setInverse <- function(inverse) i <<- inverse
    
    # getInverse function: get value of the inverse matrix
    getInverse <- function() i
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    # get inverse matrix
    i <- x$getInverse()
    
    # if cached value exists then return it.
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    # if it is null, calculate the inverse
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
