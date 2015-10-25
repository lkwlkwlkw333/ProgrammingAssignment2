## Programming Assignment 2 code
## Matrix inversion is usually a costly computation and there may 
##be some benefit to caching the inverse of a matrix rather than compute 
##it repeatedly . This R code will calculate the inverse of a matrix, and the 
## inverse of the matrix will be cached.


## This function will creates a new matrix and the inverse will the cached into memory.

makeCacheMatrix <- function(x = matrix()) {
	minverse<-NULL
	set <- function (y){
	x <<-y
	minverse<<-NULL
}
	get <- function ()x
	setminverse <- function (inverse) minverse<<- inverse
	getminverse <- function() minverse
	list (set = set, get=get,setminverse = setminverse, getminverse = getminverse)
}


## This function calculate the inverse of the matrix created by the function above.
## If the inverse value of a particular matrix has already been calculated, the CacheSolve
## program will retrive the inverse of the matrix from the cache instead of re-
## calculating it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	minverse<-x$getminverse()
	if(!is.null(minverse)){
		message("getting cached data")
		return (minverse)
	}

	matrix_new <-x$get()
	minverse <- solve(matrix_new, ...)
	x$setminverse(minverse)
	minverse
}

