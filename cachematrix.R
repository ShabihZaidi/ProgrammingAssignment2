## These functions can be used to cache a matrix and its inverse 

## makeCacheMatrix() function creates a special "matrix" object that can cache 
##its inverse
##
## set(y) -> To store data values in the matrix
## get() -> To retrieve data values in the matrix
## setinverse(inverse) -> To store the inverse of the matrix
## getinverse() -> To retrieve the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {
                x
        }
        setinverse <- function(inverse) {
                inv <<- inverse
        }
        getinverse <- function() {
                inv
        }
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## cacheSolve() function computes the inverse of the special matrix returned by 
## the makeCacheMatrix() function. If the inverse has already been calculated, 
## it simply retrieves the inverse from the cache

cacheSolve <- function(a, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- a$getinverse()
        if (!is.null(inv)) {
                message ("Retrieving from cache")
                return(inv)
        }
        data <- a$get()
        inv <- solve(data, ...)
        a$setinverse(inv)
        inv
}
