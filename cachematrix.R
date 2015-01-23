## The following fuction creates a list of functions with 4 elements:
## set: sets the matrix used in the function list.
## get: gets the matrix used in the function list.
## setinverse: set the matrix inverse of the original matrix in the function list.
## getinverse: get the matrix inverse of the original matrix in the function list.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                                     #set the inverse to NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }                                               #set the new matrix, and cleans the inv value
    get <- function() x                             #recovers the matrix
    setinverse <- function(inverse) inv <<- inverse #set the inverse
    getinverse <- function() inv                    #get the inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)                   #Creates the list with the four functions
}

## This function asummes that variable x is the list from previous function
## Tries to get the inverse of the matrix from the list
## if available it returns the value
## if not, proceeds to get the matrix, calculate the inverse and set it into x. Finally, returns the inverse.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setinverse(inv)
    inv
}
