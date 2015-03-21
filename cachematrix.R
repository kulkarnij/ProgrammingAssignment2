## Put comments here that give an overall description of what your
## functions do
## This is a collection of function that together offer a way to  speedup 
## repeated inversions of a matrix. Use makeCacheMatrix to create a cached matrix 
## from a matrix and cacheSolve to get its inverse. makeCacheMatrix assumes that the matrix 
## is invertible.
## matrices 

## Write a short comment describing this function
## Create a cached matrix from a matrix that caches its inverse. 
## The matrix is assumed to be invertible. Inverse is created lazily
## when called the first time (lazy evaluation in R)

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
	    m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) inverse <<- solve
	getinverse <- function() inverse
	list (set = set, get = get,
	      setinverse = setinverse,
	      getinverse = getinverse)

}


## Write a short comment describing this function
## Returns the inverse of a matrix. The matrix must be invertible.
## Uses pre-made value if exits, computes it if it does not.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
        	message("Returning cached inverse.")
        	return(inverse)
        }
        
        matrix <- x$get()
        inverse <- solve(matrix)
        x$setinverse(inverse)
        message("Returning computed inverse.") 
        return(inverse)
}

## matrices 

