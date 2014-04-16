## We will create special "matrix", which can cache its inverse matrix.   
## And a function which return cached inverse matrix or calculate it if cache is empty.

## makeCacheMatrix is a function which creates special 'matrix', which really is a list
## which contains 4 functions: set, get, setinv, getinv

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve returns cached inverse matrix of the matrix created via makeCacheMartix
## or calculate it if the cache is empty and put calculated value in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
