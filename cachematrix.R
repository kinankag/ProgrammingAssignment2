################################################################################
## Author: 			Kinanka Ghosh                                 ##
## Program Description:		cachematrix                                   ##
## Original Authoring Date:	26th Mar 2016                                 ##
## Last Modified Date:		26th Mar 2016                                 ##
################################################################################

## Matrix inversion is a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than computing it repeatedly  

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        # Initialize the inverted matrix as null
        m <- NULL
        
        # Set the original matrix, to what the user wants
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # Retrieves the original matrix
        get <- function() x
        
        # Sets the Inverse of Matrix in the Cache
        setInverse <- function(inverted_matrix) m <<- inverted_matrix
        
        # Retrieves the Inverted Matrix from the Cache
        getInverse <- function() m
        
        # Output is a list that helps us get the original matrix and inverted matrix
        # Along with that, we are able to set the inverted matrix
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

## Key Assumption: The matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        # Check if the value of the inverted matrix is a valid matrix
        # if yes, retrieve the inverted matrix from the cache
        
        if(!is.null(m)) {
                message("Getting the Cached Inverse Matrix")
                return(m)
        }
        
        # if no, set the data variable with the original matrix
        data <- x$get()
        
        # Compute the inverted matrix from the original matrix
        m <- solve(data, ...)
        
        # Set the value of the inverted matrix in Cache
        x$setInverse(m)
        
        # Output the inverted matrix
        m
}

###########################################################################################
## Test Run                                                                              ##
###########################################################################################
#### Create an original Matrix
# orig_mat <-  matrix( c(3, 6, 2, 3), nrow = 2, ncol = 2)

#### Create list from the original Matrix
#inv <- makeCacheMatrix(orig_mat)

#### Retrieve the original Matrix
#inv$get()

#### First Check whether there is a Cache for the given Matrix.. Expected NULL
#inv$getInverse()

#### Compute the Inverse for the given original matrix
#cacheSolve(inv)

#### Since this is the second time, it should retrieve the inverse from the cache
#cacheSolve(inv)
