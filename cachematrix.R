## Caching the Inverse of a Matrix
### Using the caching of the Inverse of a Matrix will enable to gain time for the user


# The first function makeCacheMatrix creates a special "vector", which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setMatrixInverse <- function(solve) m <<- solve
        getMatrixInverse <- function() m
        list(set = set, get = get,
             setMatrixInverse = setMatrixInverse,
             getMatrixInverse = getMatrixInverse)
}



# This function returns the inverse of the matrix. 
# It will first check if the inverse was not already computed, if it was it will get the result directly from the cache
# and skip the computation, if it was not already computed it will compute the inverse and store it in the cache thanks 
# to our first function

# Hypothesis: the matrix is always invertible.

cacheSolve <- function(x, ...) {
        m <- x$getMatrixInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setMatrixInverse(m)
        m
}


## Let's do a sample test!
##> test <- diag(2,6)
##> CachedMarix <- makeCacheMatrix(test)
##> cacheSolve(CachedMarix) 
##     [,1] [,2] [,3] [,4] [,5] [,6]
##[1,]  0.5  0.0  0.0  0.0  0.0  0.0
##[2,]  0.0  0.5  0.0  0.0  0.0  0.0
##[3,]  0.0  0.0  0.5  0.0  0.0  0.0
##[4,]  0.0  0.0  0.0  0.5  0.0  0.0
##[5,]  0.0  0.0  0.0  0.0  0.5  0.0
##[6,]  0.0  0.0  0.0  0.0  0.0  0.5



