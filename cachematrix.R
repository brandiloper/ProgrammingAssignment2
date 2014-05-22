## Write a pair of functions that cache the inverse of a matrix.
## Assume that the matrix supplied is always invertible.

## makeCacheMatrix creates a special "matrix" 
## object that can cache its inverse. the function
## returns a list of functions that do the following:
## set the value of the matrix 
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {       
 	   m <- NULL  
       set <- function(y) {    
        	    x <<- y
                m <<- NULL
        }
        get <- function() x      
        setinv <- function(inv) m <<- inv     
        getinv <- function() m           
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already been 
## calculated with the matrix being unchanged, then the function 
## should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {         
                message("getting cached data")       
                return(m)         
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m

}


## Sample run:  
 x <- matrix(rnorm(16), 4)    		## create matrix 'x'
 mat <- makeCacheMatrix(x)          ## create special 'matrix
 mat$get()  						## get the original values of matrix 
 cacheSolve(mat)  					## computes, cache and returns inverse 
 mat$getinv()						## returns inverse
 cacheSolve(mat)					## returns cahced inverse using previously computed inverse 
 
 x <- matrix(c(1, 2, 3, 4), 2, 2)
 mat$set(x)        					## modify existing matrix
 cacheSolve(mat) 				   	## computes, cache, and return  new inverse
 mat$get()						   	## returns new matrix
 mat$getinv()						## returns inverse 
 cacheSolve(mat) 					## returns cahced inverse of modified matrix