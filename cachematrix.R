## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a cache matrix which is a list of functions
## that can be applied on the matrix.
## The functions are returned as a list.
## i will contain the inverse and is initialised to NULL. 
## The set function sets the value of the vector.
## Get retrieves the value of the vector. 
## Setinverse is used to compute the inverse. Getinverse retrieves the inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
          x <<- y
          i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## cache solve

## This function tries to getinverse. 
## If there is no cached version, i which is initialised to NULL is retrieved.
## If i is not Null, it prints a message saying "getting cached data".
## It then returns the cached inverse.
## If i is NULL, the matrix is retrieved using the get function,
## and the inverse is computed using the solve function.
## Then setinverse is used to store this as the inverse.
## This newly computed inverse is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
