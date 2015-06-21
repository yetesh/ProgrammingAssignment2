##  The following are the two helper functions to avoid recomputations of 
##  inverse of a matrix if we have already cached its inverse  


##  This function takes a matrix as argument and returns a list of functions to access 
##  and modify the cached copy of matrix and its inverse
makeCacheMatrix <- function(x = matrix()) 
{
    cached_inverse <- NULL
    
    ##  function to set cached matrix to that in the argument. We need to invalidate
    ##  the previously cached inverse
    set <- function(y) 
    {
        cached_matrix <<- y
        cached_inverse <<- NULL
    }
    
    ##  function to get currently cached matrix
    get <- function() cached_matrix
    
    ##  function to set cached inverse to the newly computed inverse
    setInverse <- function(inv) cached_inverse <<- inv
    
    ##  function to get currently cached inverse
    getInverse <- function() cached_inverse
    
    list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}


##  This function takes the list of functions returned by previous function as argument
##  and returns the inverse of currently cached matrix.
cacheSolve <- function(x, ...) 
{
    ##  get the currently cached inverse and return it if it is not null
    inverse <- x$getInverse()
    if(!is.null(inverse)) 
    {
        message("getting cached data")
        return(inverse)
    }
    
    ##  if the currently cached inverse is null, get the cached matrix, compute its 
    ##  inverse, set the cached inverse to the newly computed inverse and return it
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}

#######################  Test cases ########################

##  Run the following commands in console. You need to use my makeCacheMatrix() and
##  cacheSolve() functions

##  a <- list( c(2,0),c(0,2) )
##  m <- do.call(rbind, a)
##  obj <- makeCacheMatrix(m)
##  cacheSolve(obj)
##  cacheSolve(obj)
