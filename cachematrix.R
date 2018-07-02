
## Write a short comment describing this function 
## The function, makeCacheMatrix, creates a special matrix, which is a list containing a function to
## - set the matrix
## - get the matrix
## - set the inverse matrix
## - get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        invmat <- NULL
        set <- function(y) {
                x <<- y
                invmat <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invmat <<- inverse
        getInverse <- function() invmat
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Write a short comment describing this function

# The following function calculates the inverse of the "matrix" created with the above function. 
# However, it first checks to see if the inverse matrix has already been calculated. If so, 
# it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse
# of the matrix and sets the inverse of the matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmat <- x$getinverse()
        if(!is.null(invmat)) {
                message("getting cached data")
                return(invmat)
        }
        data <- x$get()
        invmat <- solve(data, ...)
        x$setinverse(invmat)
        invmat
}
