
## The makeCacheMatrix function creates a special "matrix",
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
		z <- NULL
        set <- function(y) {
                x <<- y
                z <<- NULL
        }
        get <- function() {
			x
		}
        setinverse <- function(inverse) {
			z <<- inverse
		}
        getinverse <- function() {
			z
		}
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse
			 )
}


## The cacheSolve function calculates the inverse of the special "matrix"
## the special "matrix" which created with the makeCacheMatrix function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
## in the cache via the setinverse function and return the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inverse <- x$getinverse()
		if(!is.null(inverse)) {
			print("getting cached data")
			return(inverse)
		}
		data <- x$get()
		inverse <- solve(data,...)
		x$setinverse(inverse)
		inverse
}
