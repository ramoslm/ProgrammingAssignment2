## This pair of functions allow to calculate the inverse of a matrix and save it along with the original matrix 
## in a cache, which one is useful when need to calculate it many times.

## makeCacheMatrix return a list with 4 functions to save the inverse of a matrix in a cache
##   - set: set a matrix and inicialize it inverse
##   - get: return the matrix
##   - setinverse: set the inverse of the matrix
##   - getinverse: return the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL # initialize a variable to save the inverse
    set <- function(y) {
        x <<- y # set the matrix in the variable x, belonging to the parent environment
        i <<- NULL # initialize the variable i, belonging to the parent environment, which will contain the inverse 
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv # set the inverse in the variable i, belonging to the parent environment
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve return the inverse of a matrix
## x is a list of functions returned by the function makeCacheMatrix
cacheSolve <- function(x, ...) {
    i <- x$getinverse() # obtain the invers of the matrix
    if(!is.null(i)) { # if the invers was already calculated
        message("getting cached data") # notice that the data was obtained from the cache
        return(i)
    }
    data <- x$get() # if the inverse wasn't calculated, obtain the matrix
    i <- solve(data, ...) # calculate the inverse
    x$setinverse(i)
    i # return i directly because setinverse modify the variable in the parent environment 
}
