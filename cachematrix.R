## This is a pair fo functions that cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     ## varaiable to store the cached value
     ## initialize the variable to null
     cache <- NULL
     
     ## set value to object in environment other than
     ## working environment
     
     setfun <- function(y){
          x <<- y
          cache <<- NULL
     }
     
     # get the matrix
     getfun <- function () x
     # calc the inverted matrix and set to cache
     setinverse <- function(inverse) cache <<- inverse 
     # get the inverted matrix from cache
     getinverse <- function() cache
     #return the list of functions
     list (setfun=setfun, getfun=getfun, setinverse=setinverse, getinverse=getinverse )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          
     inv  = x$getinverse
     
     # check if inverse has already been calc
     if (!is.null(inv)){
          #use the cache and skip calc
          message("using cached value")
          return(inv)
     }
     
     #if inverse doesn't exist calc inverse
     matrix.data = x$get()
     inv = solve(matrix.data, ...)
     X$setinverse(inv)
     inv
     
     #
}
## 1st committ