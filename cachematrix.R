## I first made a function with an x argument which is a matrix. Then I assigned the NULL value to the m object.
## Within this function I created another function which assigns the input value to the object x in the parent environment
## and it also assigns the NULL value to the m object in the parent environment.

## The I created object 'get' which is a function that gets the x from the parent environment.
## Setinversion is a function that calculates the inversion of the matrix. Here I used <<- opertator to assign the
## input value to the value of 'm' in the parent environment.
## Getinversion returns the matrix inversion. 
## And finally, I created a list containing all the elements of the function and returned them to the parent environment.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinversion <- function(solve) m <<- solve
        getinversion <- function() m
        list(set = set, get = get,
             setinversion = setinversion,
             getinversion = getinversion)
}

## The second part of my code is a function which uses the argument x from the previous part of the code to calculate
## the matrix inversion. If the inversion is already computed, then R gives a message "getting cached data" and returns
## the matrix inversion value without the need for calculation. In case the inversion hasn't been calculated yet,
## the remaining part of the function is written so that is is calculated.

cacheSolve <- function(x, ...) {
        m <- x$getinversion()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinversion(m)
        m
}

## HOPE IT IS READABLE! :-)
