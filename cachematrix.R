## makeCacheMatrix creates a special
## matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

			invx <- NULL
			set <- function(y) {
				x <<- y
				invx <<- NULL
			}
		 	
			get <- function() x
			setinverse <- function(inverse) invx <<- inverse
			getinverse <- function() invx
 			list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)


}


## cacheSolve returns the inverse of a matrix created with
## the makeCacheMatrix function
## If the matrix inverse has already been calculated, cacheSolve will
## find it in the cache and return it, and not calculate it again

cacheSolve <- function(x, ...) {

			invx <- x$getinverse()
			if (!is.null(invx)) {
				message("getting cached inverse matrix")
				return(invx)
			}
			else {
				invx <- solve(x$get())
				x$setinverse(invx)
				return(invx)
			}

}













