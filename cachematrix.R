## The functions here create a special matrix object that can cache its
## inverse, then retrieve the inverse from the cache, or calculate the 
## inverse if it has not been solved or if the matrix has changed.
## The functions assume a square matrix that is invertible.

## This function creates the matrix object and provides functionality
## to change the matrix object. It also sets and retrieves the inverse
## of the matrix

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinverse <- function(inv_x) inv <<- inv_x
     getinverse <- function() inv
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
     ## get value of inv using getinverse
     inv <- x$getinverse()
     ## check to see if value is not null, meaning inverse has already
     ## been calculated and cached. If cached return cached value and end.
     if (!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
        ## if inv is null we have to calculate the inverse
     data <- x$get()
     inv <- solve(data)
     x$setinverse(inv)
     inv
}

