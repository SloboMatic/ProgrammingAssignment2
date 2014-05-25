## The following two functions compute or retrieve the previously cached inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
## It uses the <<- operator to assign a value to an object outside of the current 
## environment.

## Try: z1=makeCacheMatrix(diag(3))
## Or:  z2=makeCacheMatrix(matrix(c(1,3,2,4),2,2))

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Assumption: input argument x is an invertible matrix

## Try: cacheSolve(z2)%*%z2$get()
## Or:  z1$get()%*%cacheSolve(z1)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
