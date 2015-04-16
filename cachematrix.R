## makeCacheMatrix function is used to create a special object 
## that stores a matrix(here is square matrix) and caches its inverse.
## cacheSolve function is used to get the inverse cached in memory or 
## calculate a new inverse and cache the inverse to the special object
## above.

## Create a special object that stores a matrix and caches its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Cache the inverse of the matrix made by makeCacheMatrix function
## or calculate the inverse first time.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
    inv    
}
