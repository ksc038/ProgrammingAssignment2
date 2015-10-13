## Programing Assignment 2: Lexical Scoping

## These R function caches the potentially time-consuming computations
## of inversing matrixes. If the matrix is not changed, than rather than recompute 
## the inverse these functions make you able to look up the value in the cache from
## the first computation of the inverse.  

## This function creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix())
		## makeing a list containg a function to
		## 1. set the matrix
		## 2. get the matrix
		## 3. set the inverse
		## 4. get the inverse
		
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the
## cacheSolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv = x$getinv()
        ## if the inverse has already been computed then skip the
        ## computation and return the inverse. 
        if (!is.null(inv)){ 
                message("getting cached data")
                return(inv)
        }
        ## if not, compute the inverse...
        mat.data = x$get()
        inv = solve(mat.data, ...)
        ## ...and set the inverse in the cache and return the result
        x$setinv(inv)
       
        return(inv)
}
