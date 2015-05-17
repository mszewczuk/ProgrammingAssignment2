## The functions allow to create and operate a 'enhanced' version of a matrix class, which is able
## to cache its inverse for future resue, once calculated.
##
## makeCacheMatrix - creates the instance of the 'enhanced' matrix class
## cacheSolve - calculated the inverse of the 'enhanced' matrix and caches the result
## 

##
## A function that creates the special class of matrix, which calculates inverse of itsself when requested,
## and caches the calculated value, to avoid re-calculations. The cache is cleared when the initial matrix
## is replaced.
##
makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    
    get <- function() {
        x
    }
    
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    
    setToCache <- function(y) {
        cache <<- y
    }
    
    getFromCache <- function() {
        cache
    }
    
    list(set = set, get = get, setToCache = setToCache, getFromCache = getFromCache)
}


##
## The method calculates the inverse of the given matrix 'x'. Once the inverse is calculated, it's cached to avoid
## additional re-calculations.
##
## The method accepts the instance of special class "cacheMatrix" created by makeCacheMatrix method.
## 
cacheSolve <- function(x, ...) {

    inverse <- x$getFromCache()
    
    # if the cache contains the calculate inverse value, return it
    if(!is.null(inverse)) {
        
        return(inverse)
    }
    
    data <- x$get()
    
    inverse <- solve(data, ...)
    
    x$setToCache(inverse)
    
    inverse
}
