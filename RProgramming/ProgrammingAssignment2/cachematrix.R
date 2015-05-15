##makeCacheMatrix stores a cached matrix and a inverse matrix for a given value
##makeCacheMatrix will be useful to reference an already calculated inverse matrix

##Contains getters and setters for matrix and inverse matrix

makeCacheMatrix <- function(x = matrix()) {

	z <- NULL
	set <- function(y){
		x <<- y
		z <<- NULL
	}

	get <- function(){
		x
	}

	setInverse <- function(inverse){
		z <<- inverse
	}

	getInverse <- function(){
		z
	}
	list(set=set, get=get, setInverse = setInverse, getInverse = getInverse)


}


## Original function where the inverse matrix is referred if any is existing
## If inverse of a matrix is present, it returns the inverse 
##otherwise the inverse is calculated and set to the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    z <- x$getInverse()
    if(!is.null(z)){
    	message("caching matrix data");
    	return(z)
    }
    data <- x$get()
    z <- solve(data,...)
    x$setInverse(z)
    z
}