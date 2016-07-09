## The following functions calculate the inverse of a matrix and saves it 
## to cache so that the next time the user needs to calculate the 
## matrix inverse, the previously cached value is returned instead of 


## This function creates a special "matrix" object, which is really a list  
## containing a function to 
## 1. set the value of the matrix 
## 2. get the value of the matrix 
## 3. set the value of the inverse 
## 4. get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
	## create the 'matrix' object
	m <- NULL 
         set <- function(y) { 
             x <<- y ## assign the input matrix y to the variable x in the 
                 ## parent environment 
             m <<- NULL ## set m in the parent environment to null 
         } 
         get <- function() x ## return the matrix x 
         setinverse <- function(inverse) m <<- inverse ## set the cache m equal 
         ## to the inverse of the matrix x 
         getinverse <- function() m ## return the cached inverse of x 
         list(set = set, get = get, 
              setinverse = setinverse, 
              getinverse = getinverse)
}


## This function calculates the inverse of the special "matrix" created 
## using the makeCacheMatrix function. It first checks to see if the inverse 
## has already been caclulated. If it has been calculated, it skips the calcualtion and
## 'get's the inverse from the cache. Otherwise, it calculates the matrix inverse 
## and sets the value of the inverse in the cache using the 'setinverse' function
## and returns the matrix 'm'.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse() 
         if(!is.null(m)) { 
                 
                 return(m) 
         } 

         data <- x$get() 
         m <- solve(data, ...) 
         x$setinverse(m) 
         m
}
