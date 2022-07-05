setwd('C:/Users/monte/Desktop/Cousera R/Programming Assignment 2/ProgrammingAssignment2')

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        set <- function( matrix ) {
                m <<- matrix
                i <<- NULL
        }
        
        get <- function() {m}
        
        setInverse <- function(inverse) {i <<- inverse}
        
        getInverse <- function() {i}
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        
        
        if( !is.null(m) ) {
                message("Retaining cached data")
                return(m)
        }
        
        data <- x$get()
        
        m <- solve(data) %*% data
        
        x$setInverse(m)
        
        m
}
