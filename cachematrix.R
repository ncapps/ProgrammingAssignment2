### Assignment: Caching the Inverse of a Matrix



## This function creates a special 'matrix' object that 
## can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL ## Initialize objects
        
        ## whenever x is reset, the value of m cached in the memory
        ## of the object is cleared
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## x is retrieved from parent environment makeCacheMatrix
        get <- function() x 
        
        ## Setter for the inverse i
        ## <<- assigns input argument to value of i in parent environment
        setinverse <- function(inverse) i <<- inverse
        
        ## Getter for the inverse i
        ## Uses lexical scoping to get value of i in parent environment
        getinverse <- function() i
        
        # gives the name 'set' to the set() function defined above
        # gives the name 'get' to the get() function defined above
        # gives the name 'setinverse' to the setmean() function defined above
        # gives the name 'getinverse' to the getmean() function defined above
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...) ## Compute inverse using solve function
        x$setinverse(i)
        i
}
