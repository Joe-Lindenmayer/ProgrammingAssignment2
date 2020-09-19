## The purpose of this project/these functions is to show how we can
## cache data in order to skip computations we have already performed
## for the interest of saving time and computer power

## This function creates a special vector, which is really a list to define
## setting the value of a matrix, getting the value of a matrix, setsolve
## to produce the inverse of a matrix, and getsolve to get the inverse of
## an already computed inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
            x <<- y
            m <<- NULL
        }
        get <-function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## This function checks to see if the input matrix has been inverted and
## stored already. If it was computed, user receives a message and the 
## program returns m. If the matrix had not yet been inverted, then the
## matrix is solved normally and the result is cached for future use

cacheSolve <- function(x, ...) {
       m <- x$getsolve()
       if(!is.null(m)) {
         message("getting cached data")
         return(m)
       }
       data <- x$get()
       m <- solve(data, ...)
       x$setsolve(m)
       m
}

