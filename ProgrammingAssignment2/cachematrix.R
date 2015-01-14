## The following functions should be used to cache and get the inverse of a matrix.
## As stated in the assignment, we will assume that the matrix supplied is always invertible.
## To use it, the first step is to pass a matrix as an argument to the "makeCacheMatrix" function, and assign it to a variable.
##
## Usage example:
##
## Create a square matrix and pass it as an argument to the "makeCacheMatrix" function:
## x <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2))
##
## Pass the resulting object as an argument to the "cacheSolve" function to get the inversed matrix:
## cacheSolve(x)


## This function "makeCacheMatrix" creates a special matrix, which is really a list containing a function to
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of the inversed matrix
##   4. get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {

	s <- NULL

	set <- function(y){
		x <<- y
		s <<- NULL
	}
	
	get <- function(){
		x
	}
	
	setSolve <- function(solve){
		s <<- solve
	}
	
	getSolve <- function(){
		s
	}
	
	list(
		set = set,
		get = get,
		setSolve = setSolve,
		getSolve = getSolve
		)

}


## This function "cacheSolve" calculates the inversed matrix of the special matrix created with the above function.
## However, it first checks to see if the inversed matrix has already been calculated.
## If so, it gets the inversed matrix from the cache and skips the computation.
## Otherwise, it calculates the inversed matrix of the data and sets the value of the inversed matrix in the cache via the setSolve function.

cacheSolve <- function(x, ...) {
	
	s <- x$getSolve()
	
	if(!is.null(s)){
		message("Getting cache data:")
		return(s)
	}
	
	data <- x$get()
	
	s <- solve(data, ...)
	x$setSolve(s)
	s
}