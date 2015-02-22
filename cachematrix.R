## Creation of a mechanism to cache, retrieve, and store a matrix and its
## inverse

## makeCacheMatrix, takes in a matrix(x), and returns a list of function
## that will provide access to the matrix and its inverse(1)

makeCacheMatrix <- function(x = matrix()) {

	i<-NULL
	set <- function(y){
		x<<-y
		i<<-NULL
	}##set inverse to be NULL until it has been set via setinv()

	get <- function() x
	setinv <- function(s) i<<-s
	getinv <- function() i

	list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
	##return accessor functions

	##getinv() return cached inverse
	##get() return matrix
	##setinv()
}	##set() save matrix


## cacheSolve will take in an object that is of the special type,
## created by makeCacheMatrix, it will check if the matrix inverse is cache
## it will return the cached inverse, or compute, cache, and return the inverse 

cacheSolve <- function(x, ...) {

      ## check if inverse is already cached
	s<-x$getinv()
	if(!is.null(s)){
		message("getting cached data")
		return(s)
		##return already cached inverse
	}

	data<-x$get()
	s<-solve(data)

	x$setinv(s)
	##cache inverse

	s
	##return inverse
}
