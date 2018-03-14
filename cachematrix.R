## makeCacheMatrix creates a special matrix object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # values are undefined
        set <- function(y) {
                x <<- y 
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve 
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)   
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
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

## make example data 
a=c(1,0,5,2,1,6,3,5,0)
b=matrix(a, 3,3)
## run the functions
make=makeCacheMatrix(b)
cacheSolve(make)
cacheSolve(make)
