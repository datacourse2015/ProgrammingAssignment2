        ##Peer Assessments /Programming Assignment 2: Lexical Scoping 
        ## **Please Note: No Grace Period**
        
        ## The following functions makeCacheMatrix & cacheSolve, together 
        ## create a square invertible matrix, and then return the 
        ## inverse of the matrix available from the cache environment


        ## makeCacheMatrix creates an invertible matrix,
        ## returning a list containing functions to:
        ##              1. set the value of the matrix
        ##              2. get the value of the matrix
        ##              3. set the value of the inverse
        ##              4. get the value of the inverse
        ## this list is used as the input to cacheSolve()
        
makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) cache <<- inverse
    getinverse <- function() cache
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

        ## cacheSolve computes the inverse of the “matrix” returned by 
        ## makeCacheMatrix(). If the inverse is already calculated with
        ## no change to the matrix, cacheSolve retrieves the inverse 
        ## from the cache

cacheSolve <- function(x, ...) {        
        ## Return a matrix that is the inverse of 'x'
    cache <- x$getinverse()
    if(!is.null(cache)) {
        message("getting cached data")
        return(cache)
    }
    data <- x$get()
    cache <- solve(data, ...)
    x$setinverse(cache)
    cache
}
}
