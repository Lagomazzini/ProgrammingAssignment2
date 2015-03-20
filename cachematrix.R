## These two functions "makeCacheMatrix()" and "cacheSolve()" make an "special" object that 
## store a matrix and cache its inverse. Example at the end.


## The following function creates a list containing functions that set and get the
## argument "x" which is a matrix (set and get functions, respectively), and 
## set and get the inverse of that matrix (setinverse and getinverse functions, respectively)

makeCacheMatrix <- function(x = matrix()) {
        i<- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x    
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(set=set, 
             get=get, 
             setinverse = setinverse, 
             getinverse = getinverse)    
}


## The following function computes the inverse of the matrix cached in the special object
## created above. If the inverse of the matrix was cached already (and the matrix did not
## change) the value of the inverse is retrieved and a message will appear to indicate it.

cacheSolve <- function(a, ...) {
        i <- a$getinverse()
        if ( !is.null(i) ){
                message("Getting cached inverse of the matrix (the matrix did not change)")
                return(i)
        }
        data <- a$get()
        i <- solve(data, ...)
        a$setinverse(i)
        i
        ## Return a matrix "i" that is the inverse of 'x'
}

#   Example:
#       > x<-matrix(1:4,2,2)
#       > source("cachematrix.R")
#       > b<-makeCacheMatrix(x)
#       > cacheSolve(b)
#             [,1] [,2]
#       [1,]   -2  1.5
#       [2,]    1 -0.5
#       > cacheSolve(b)
#       Getting cached inverse of the matrix (the matrix did not change)
#             [,1] [,2]
#       [1,]   -2  1.5
#       [2,]    1 -0.5
    
        
