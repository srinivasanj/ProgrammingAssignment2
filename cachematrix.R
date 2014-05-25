## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
## This R Code allows you to cache the inverse of a matrix that you are working with and then work with this cached value. The inverse of your matrix is calculated only if the inverse is not cached already



# # The first function, makeCacheMatrix gives you four functions for your matrix
# # 1.	set the value of the matrix: For example mat$set(matrix(1:4,2)) creates a matrix called 'mat' which is a 2 x 2 matrix with the elements 1,2,3,4
# # 2.	get the value of the matrix: This will get the value of the matrix
# # 3.	set the value of the inverse of the matrix: Not really used by the user but used by the second function
# # 4.	get the value of the inverse of the matrix: This will get the inverse of the matrix
## NOTE: To get the inverse of your matrix you need to run the second function cacheSolve on your matrix. Otherwise, the value for the inverse will be returned as NULL


## This function provides the user with the functions to get the value of the matrix and define the value for the matrix (using set). You can also call the getInverse function to get the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {

# matInv will store the cached inverse matrix
# Its initialised to NULL. When the cacheSolve function is called, the value for matInv is set as the inverse of your matrix
    matInv <- NULL
    
# Set your matrix
    set <- function(y) {
        x <<- y
        matInv <<- NULL
    }
    
# Get your matrix
    get <- function() x
    
# Call this to set the inverse
# NOTE: Not really used by the user but more by the second function.
    setInverse <- function(inverse) matInv <<- inverse
    
# Call this to get the inverse: For example myMatrix$getInverse()
    getInverse <- function() matInv

# List of functions 
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This functions returns the inverse of the matrix passed into it
## If the inverse of the matrix is available in the cache, a message is printed out and the value is returned
## If the inverse of the matrix is not available in the cache, the value is calculated and returned

cacheSolve <- function(x, ...) {

## Return a matrix that is the inverse of 'x'
matInv <- x$getInverse()

    # If the inverse of you matrix is already available, return that value
    if (!is.null(matInv)) {
        message("getting cached data")
        return(matInv)
    }

    # If the inverse of the matrix is not available, the value is calculated and set to you matrix inverse variable
    tmpMatrix <- x$get()
    matInv <- solve(tmpMatrix)

    # Set the inverse into the cache
    x$setInverse(matInv)

    # Return the value of the inverse of the matrix
    matInv

}
