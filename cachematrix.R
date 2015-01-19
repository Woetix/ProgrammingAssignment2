##  A set of functions that calculates the inverse of a matrix, and caches the result, so that when the inverse is needed 
## again, the cache is used instead of doing the calculation again

## Creating a list with commands and environment including the matrix and its inverse, taking the original matrix as input

makeCacheMatrix <- function(x = matrix()) {
                i <- NULL    ##set the inverse to NULL 
                set <- function(y) {         ##create the set function and flush i in case the $set function is called
                        x <<- y              ##assuming the only way to change the matrix is by using $set
                        i <<- NULL           
                }
                get <- function() x          ## creating a function that simply gets the matrix
                setinverse <- function(inverse) i <<- solve(x) ## creating a function that calculates the inverse of the matrix
                getinverse <- function() i    ## creating a function that gets the inverse
                list(set = set, get = get,     ##creating the list with commmands
                     setinverse = setinverse,
                     getinverse = getinverse)
}


## gettting the inverse of x  if it is already calculated, or calculate if not the case.

cacheSolve <- function(z, ...) {
        ## Return a matrix that is the inverse of 'x'
         i <- z$getinverse()    ## get the inverse out of the list created by makeCacheMatrix, by callingt the 
                if(!is.null(i)) {      ## getinverse function. then testing if the cache is empty or not 
                        message("getting cached data") ## if not: get the previously calculated inverse
                        return(i)
                }
                data <- z$get()  ## if cache is empty, get the original data 
                i <- solve(data, ...) ## calculate the inverse,
                z$setinvers(i)        ## write it back into the environment
                i                     ## and print to console
}
