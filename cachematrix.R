## Week 3 Assignment. R Programming. August 2014

setwd("F:/GW/tmp/Tocopy/2_Rprog/homework2014")


# Creates a special "matrix" object that can cache its inverse
# The matrix is assumed to be invertible
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}


## GW. Tests inspired from discussion forum
# a_matrix <- matrix(c(2,0,0,0,2,0,0,0,2), 3, 3)
# a <- makeCacheMatrix(a_matrix)
# a$get()            # Prints the above matrix
# a$getinverse()     # Returns: NULL
# cacheSolve(a)      # Computes inverse
# a$getinverse()     # to show that the inverse has been stored and does not affect anything
# cacheSolve(a)      # Restores the inverse from cached data


