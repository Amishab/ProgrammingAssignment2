## Programming Assignment2 by Amisha Bhanage


## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly


## Following are a pair of functions that
## cache the inverse of a square matrix


## The first function, "makeCacheMatrix" creates a special "matrix", 
## which contains a list containing a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of the matrix
## 4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      
        inv <- NULL
        
        ## set the value of the matrix
        set <- function(y) {
          x <<- y
          inv <<- NULL
        }
        
        ## get the value of the matrix
        get <- function() x
        
        ## set the value of the inverse of the matrix
        setinverse <- function(inverse) inv <<- inverse
        
        ## get the value of the inverse of the matrix
        getinverse <- function() inv
        
        
        ## create a list of functions available to users
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
  
}


## The following function returns the inverse of the special "matrix" 
## created with the "makeCacheMatrix" function above. However, it first checks to see 
## if the inverse has already been calculated (and the matrix has not changed). 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and 
## sets the value of the inverse of matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
          
        ## check if the inverse has already been calculated 
        ## then get the cached data using getinverse() function 
        ## from makeCacheMatrix
        inv <- x$getinverse()
        
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        
        ## This code runs if cached data is not available
        
        message("calculating inverse of matrix since cached data is not available")
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        
  
        ## Return a matrix that is the inverse of 'x'
        inv
}



## Example of getting cached inverse of matrix

# create a square numeric matrix
mdat <-matrix(c(11, 21,12, 22), nrow=2, ncol=2)
mdat
## Cosnole Output
##       [,1] [,2]  
## [1,]    11   12
## [2,]    21   22

# Construct a cached matrix using function makeCacheMatrix
makemat <- makeCacheMatrix(mdat)

#first time you run function cacheSolve (no cached data available)
cacheSolve(makemat)
## Cosnole Output
# calculating inverse of matrix since cached data is not available
#      [,1] [,2]
# [1,] -2.2  1.2
# [2,]  2.1 -1.1


#Next time you run function cacheSolve (cached data available)
cacheSolve(makemat)
## Cosnole Output
# getting cached data
#      [,1] [,2]
# [1,] -2.2  1.2
# [2,]  2.1 -1.1




