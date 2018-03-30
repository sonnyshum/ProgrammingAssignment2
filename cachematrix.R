## This R file contains two functions:
##     makeCacheMatrix(x=matrix())
##     cacheSolve(cm) # where cm is the object returned by makeCacheMatrix

## This function takes a matrix as input and returns an object that can
## cache the value of its inverse. The return object also contains a list of
## accessible helper functions (set,get,setinverse,getinverse)

makeCacheMatrix <- function(x = matrix()) {
	xinv <- NULL
	set <- function(y) {
		x <<- y
		xinv <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) xinv <<- inv
	getinverse <- function() xinv
	list(set = set, get = get,
	     setinverse = setinverse, getinverse = getinverse)
}

## This function takes as input the object returned by makeCacheMatrix and
## return the inverse matrix of the input matrix. If an inverse matrix was
## previously computed then it is returned otherwise a new one is computed and
## cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(is.null(inv)) {
        	x$setinverse(solve(x$get()))
		inv <- x$getinverse()
	}
	inv
}
