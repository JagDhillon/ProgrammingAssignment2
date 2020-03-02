## Implementation of a special matrix which caches its inverse.
## This is more efficient as matrix inverse calculation is an 
## expensive operation


## makeCacheMatrix creates a special matrix from an ordinary matrix
makeCacheMatrix <- function(x = matrix()) {
	## invx stores the inverse of the matrix
	invx <- NULL

	##Initiallise the matrix and its mean
	set <- function( amatrix) {
		x <<- amatrix
		invx <<- NULL
	}
	##Get the matrix
	get <- function() x

	##Set the matrix inverse
	setinv <- function( inv) invx <<- inv
	##Get the matrix inverse
	getinv <- function() invx
	list( set=set, get=get,
		setinv = setinv,
		getinv = getinv)
}


## cacheSolve takes in a special matrix and returns it cached inverse,
## or calculates its inverse and caches it.
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	invmat <- x$getinv()
	if ( !is.null(invmat) ) {
		message("getting cached inverse")
		return( invmat)
	}
	mat <- x$get()
	invmat <- solve( mat, ...) ##Assumption: Inverse of mat exists
	x$setinv( invmat)
	invmat	
}
