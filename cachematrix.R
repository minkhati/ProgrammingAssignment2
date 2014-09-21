makeCacheMatrix <- function(x = matrix()) {
        # initially cache with NULL
        cache <- NULL
        
        # Set a matrix
        setMatrix <- function(matrix) {
            	# assign matrix to the new environment variable mat
               	mat <<- matrix
                # Assign NULL to cache to indicate matrix is just set
                cache <<- NULL
        }

        # returns the stored matrix
        getMatrix <- function() {
                mat
        }

        # method to set the inverse matrix 
        cacheInverse <- function(inverse) {
                cache <<- inverse
        }

        # get the cached inverse matrix
        getInverse <- function() {
                cache
        }
        
        # return a list. Each named element of the list is a function
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


# The following function calculates the inverse of a special matrix 
# created with makeCacheMatrix
cacheSolve <- function(x, ...) {
        # get the cached value
        invmat <- x$getInverse()
        # if a cached value exists return it
        if(!is.null(invmat)) {
                message("getting cached data")
                return(invmat)
        }
        # otherwise get the matrix, caclulate the inverse and 
        # store it in the cache
        mat <- x$getMatrix()
        invmat <- solve(mat)
        x$cacheInverse(invmat)   
}