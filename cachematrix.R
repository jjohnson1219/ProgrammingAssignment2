## The functions makeCacheMatrix and cacheSolve cache the 
## inverse of a matrix

## makeCache creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## If inverse of "matrix" returned by makeCachematrix has
## been calculate, cacheSolve retrieves it.  
## If not, it computes the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached inverse")
                return(m)
        }
        data  <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}
