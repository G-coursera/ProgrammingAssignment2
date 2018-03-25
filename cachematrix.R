##Creating CacheMatrix from a matrix
makeCacheMatrix <- function(X=numeric()){
        
        inv <- NULL
        
        setmatrix <- function(Y){                     ##Setting Matrix 
                X <<- Y
                inv <<-NULL
        }
       
        getmatrix <- function() X                     ##Getting Matrix
        
        setinv <- function(inverse) inv <<- inverse   ##Setting the inverse Cache
        getinv <- function() inv                      ## Getting the inverse Cache
        
        ##Making list of functions for the cache
        list(setmatrix=setmatrix, getmatrix=getmatrix, 
             setinv=setinv, getinv=getinv)
        
}


##Checking cache / calculating the inverse of a CacheMatrix
cacheSolve <- function(X,...){

        inv <- X$getinv()                         ##getting value for inverse of matrix
        data <- X$getmatrix()                     ##getting data
        
        ##If cache contains inverse
        if(!is.null(inv)){                        ##Perform check for inverse
                message("Getting cached data")
                return(inv)                       ##returning inverse from cache
          
        }
        ##If cache is not used
        inv <- solve(data, ...)                   ##Inverse is calculated
        X$setinv(inv)                             ##Cached inverse is set
        inv                                       ##Inverse is returned
        
        
}