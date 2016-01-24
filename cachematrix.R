## Put comments here that give an overall description of what your
## functions do

##Function “makeCacheMatrix” creates a special matrix object that can cache its inverse. 
##makeCacheMatrix contains 4 functions: set, get, setInverse, getInverse.
##get is a function that returns the vector x stored in the main function.
##set is a function that changes the vector stored in the main function.
##setInverse and getInverse are functions that will help us get and set the inverse with the cache function, 
##begins inv equals null in order that cache function can know if there is a cache inv or not.

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(newMatrix) {
                x <<- newMatrix
                inv <<- NULL
        }
        get <-function() x
        setInverse <-function(solve) inv<<-solve
        getInverse <-function() inv
        list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}


## This function manipulates the values of the inverse of matrix, first try to get the valur of inv and
##already exists then just get and returnit but if its null calculate it with the solve function

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

