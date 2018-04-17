## This function is used to get the inverse of a matrix when it was previously 
## executed, using data in cache, or to calculate it and store in cache, 
## for future use. SAving processing resources.

## This function is determining the objects from the functions and 
## it's interations like detemine the object for the matrix, the enviroment
## store and load operations. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function is access the cache to check the matrix index, if previous
##  previous calculated, or generating the matrix inverse if the cache is empty.
## As the operation is doing the treatment for NA values, and printing the final
## result and one indicator if the data was in cache.

cacheSolve <- function(x, ...){
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
        ## Return a matrix that is the inverse of 'x'

