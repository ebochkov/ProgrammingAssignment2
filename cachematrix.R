## This source file defines two functions for  
## matrix inversion with caching functionality.
## 'makeCacheMatrix' function creates a wrapper
## object for storring and accessing to the original
## and inverted matrix
## 'cacheSolve' function calculates inversed matrix by getting 
## cached result if it's available or actual calculating using
## 'solve' function.
 

## Creates a wrapper object for accessing original and inverted matrix.
## Arguments 
## original.matrix - matrix that should be inverted.
## Returns 
## List of methods for accessing original and inverted matrix.
makeCacheMatrix <- function(x = matrix()) {
	original.matrix <- x
	inversed.matrix <- NULL
        set.original.matrix <- function(y) {
                original.matrix <- y
                inversed.matrix <<- NULL
        }
        get.original.matrix <- function() original.matrix 
        set.inversed.matrix <- function(ix) inversed.matrix <<- ix
        get.inversed.matrix <- function() inversed.matrix
        list(set.original.matrix = set.original.matrix,
	     get.original.matrix = get.original.matrix,
             set.inversed.matrix = set.inversed.matrix,
             get.inversed.matrix = get.inversed.matrix)
}


## Calculates inversed matrix by getting cached result if it's 
## available or actual calculating using 'solve' function.
## Arguments 
## x - wrapper object, created by 'makeCacheMatrix' function 
## Returns
## Inverted matrix of the original one.
cacheSolve <- function(x, ...) {
	ix <- x$get.inversed.matrix()
        if(!is.null(ix)) {
                message("getting cached data")
                return(ix)
        }
        original.x <- x$get.original.matrix()
	dim(original.x)
        ix <- solve(original.x, ...)
	x$set.inversed.matrix(ix)
	ix
}
