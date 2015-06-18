## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function serves to cache the input matrix that can be used by "cacheSolve" function
## in case the inverse matrix must bu computed
## Also this function define the set function that allow to set the inverse of input matrix
## and to test if the inverse of input matrix. In fact, if the input matrix changes
## the variable matrix "inverse" is set to NULL using "<--" in order to be used as input for
## the CacheSolve function
## All this functions are needed by "cacheSolve" function to decide if using cached data or
## make a new computation

makeCacheMatrix <- function(matr = matrix()) {
        inverse <- NULL 
        set <- function(y) {
                matr <<- y
                inverse <<- NULL
        }
        get <- function() matr
        set_inverse <- function(inverse_matrix) inverse <<- inverse_matrix
        get_inverse <- function() inverse
        
        list(get = get, set = set, get_inverse = get_inverse, set_inverse = set_inverse)
        
}


## Write a short comment describing this function
## This function first verify if the inverse of matrix in input of makeCacheMatrix has
## been computed. If this is the case it returns the cached data.
## If not, it uses the input matrix of makeCacheMatrix (via x$get()) to compute the inverse.

cacheSolve <- function(x, ...) {
        inv_matr <- x$get_inverse()
        if(!is.null(inv_matr)) {
                message("getting cached data for inverse matrix")	
                return(inv_matr)
        }
        matrice <- x$get()
        inv_matr <- solve(matrice)
        x$set_inverse(inv_matr)
        inv_matr
        ## Return a matrix that is the inverse of 'x'
}