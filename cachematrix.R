## Cache Matrix
## Author: Eric Yablunosky
## Date: 19 March 2015
## Course: R Programming (Coursera)
##
## The functions in this source file are used to create and get a special type of 'matrix'
## This 'Matrix' caches its inverse value after it is first computed, to save computation
## effort if the inverse of the matrix is called often.

## makeCacheMatrix
## args:: matrix.cache: Square, invertable matrix to cache.
## Makes a special type of 'Matrix' which can cache its inverse to reduce computations.
makeCacheMatrix <- function(matrix.input = matrix()) {
    inverse.cached <- NULL
    
    ## Get / Set objects for the matrix.
    matrix.set <- function(mtx){
        matrix.input <<- mtx
        inverse.cache <<- NULL
    }
    matrix.get <- function() matrix.input
    
    ## Get / Set objects for the inverse.
    inverse.set <- function(matrix.inverse) inverse.cached <<- matrix.inverse
    inverse.get <- function() inverse.cached
    
    ## Return a list of the getters and setters.
    list(matrix.set = matrix.set, matrix.get = matrix.get, 
         inverse.set = inverse.set, inverse.get = inverse.get)
}


## cacheSolve
## args:: matrix.input: special 'Cache Matrix' which caches its inverse
## Returns the cached value of the inverse of a 'Cache Matrix',
##      or computes and caches the inverse if not yet set.
cacheSolve <- function(matrix.input, ...) {
    inverse.solved <- matrix.input$inverse.get()
    
    ## Case when inverse is already cached.
    if(!is.null(inverse.solved)) {
        message("Getting cached data")
        return(inverse.solved)
    }
    
    ## Case when inverse must be computed and cached.
    mtx <- matrix.input$matrix.get()
    inverse.solved <- solve(mtx, ...)
    matrix.input$inverse.set(inverse.solved)
    
    ## Return the inverted matrix.
    inverse.solved   
}
