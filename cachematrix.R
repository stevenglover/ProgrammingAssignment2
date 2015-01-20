## This set of functions compute the inverse of a matrix, x, and caches the result to save computational power if recalculating the inverse of the same matrix

## This function creates a matrix that can cache its inverse, "inv"

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL ## * if matrix changes after running the set() function, the cached value will be cleared and 'cacheSolve' will calculate new value
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function inverts the matrix from makeCacheMatrix, if not already cached
## Returns the cached value if has already been calculated and matrix is unchanged (see*)

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinv(inv)
        inv
}
