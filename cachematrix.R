## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
## This creates a cachematrix object where x is defined as a matrix
makeCacheMatrix <- function(x = matrix()) {
        ## NULL is assigned the variable m
        m <- NULL
        ## Sets the value of the matrix
        set <- function(y) {
                ## y is superassigned x which is defined as a matrix
                x <<- y
                ## NULL is superassigned to m
                m <<- NULL
        }
        ## Get the value of the matrix depending on the supplied vector
        get <- function() x
        ## Sets the value and gets the value of the inverse
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
}

## cacheSolve function takes the arguments x, in this case a matrix,and ... 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## gets the inverse of the matrix and assigns it to m
        m <- x$getinverse()
        
        ## if m is NOT a null value, previously solved, then print message
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## otherwise assign the get function a name 'data'
        data <- x$get()
        ## and passes in 'data' as an argument in the solve function assigned to m
        m <- solve(data, ...)
        ## ...which sets the inverse of the cached matrix 
        x$setinverse(m)
        m 
}

