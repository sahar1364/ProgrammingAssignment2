#The first following functions is responsible for creation of a matrix object that enables caching of some values (e.g. inverse) on it to improve performance and the second function is responsible for calculating the inverse of the given matrix object (if it is cached alreay it will return the cached value else it will calculate it)

#this function is responsible for creating a matrix object and returns a list of functions that can be used to set and get the matrix and set and get the inverse of the matrix which can be used for caching the inverse
makeCacheMatrix <- function(x = matrix()) {
    #inverse keeps the value of the matrix inverse
    inverse <- NULL
    #setter for the matrix x
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    #getter returns the matrix x
    get <- function() x
    #setter sets the inverse value for the matrix
    setInverse <- function(newInverse) inverse <<- newInverse
    #getter gets the inverse value for the matrix
    getInverse <- function() inverse
    #return a list of the getters and setters
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
}


#this function is responsible for calculating the inverse of a given matrix object (if its cached already it will return it) and cache it if it was not cached alreay
cacheSolve <- function(x, ...) {
    #get the existing value of inverse on the matrix object
    inverse <- x$getInverse()
    #if it is not null then return it
    if (!is.null(inverse)) {
        message("getting cached inverse value for the matrix")
        return(inverse)
    }
    #if it is null then get the matrix and calculate the inverse value by calling solve(x)
    matrix <- x$get()
    inverse <- solve(matrix)
    #set the inverse on the matrix object so it is cached
    x$setInverse(inverse)
    #return the inverse value
    inverse
}