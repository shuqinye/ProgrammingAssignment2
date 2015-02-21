# Description of what these two functions do:

# These two functios are to cache potentially time-consuming computation to find out
# the inverse of a square matrix. For a short matrix, the operation maybe fast and can be
# easily computed by using the solve() function. However, for a matrix with large
# dimensions, it may take too long. If the content of the matrix is not changing, 
# its inverse can be looked up in the cache rather than re-computed. These two functions
# combined does this job and look up the inverse of a matrix in the cache before
# deciding whether to compute the inverse. Thus, it increases execution speed and saves
# time.

#### FIRST FUNCTION ####

# The first function, makeCacheMatrix creates a special "matrix"
# which is a list containing a function to:

# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


#### SECOND FUNCTION ####

# The second function, cacheSolve, calculates the inverse of the special
# "matrix" created with the first function. However, it first checks to see if the 
# inverse has already been calculated. If so, it gets the inverse from the cache and 
# skips the computation. Otherwise, it calculates the inverse of the data and sets 
# the value of the inverse in the cache via the setinverse function.


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
