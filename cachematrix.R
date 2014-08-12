# Code for Assignment 2 as part of the Programming in R Coursera course

# Code utilises the framework provided in assignment details

# The first function, makeCacheMatrix  creates a special "matrix", which is really a list containing a function to # 1.set the value of the matrix # 2.get the value of the matrix # 3.set the inverse of the matrix # 4.get the inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {

    #Resetting the IM each time function is called
        im <- NULL

    #Sets matrix to what has been passed into function
        set <- function(y) {
                x <<- y
                im <<- NULL
        }

    #Sets the get() call to be the matrix as specified in above funtion
        get <- function() x

    #Sets matrix to the inverse of matrix which has been passed into function
        setinv <- function(solve) im <<- solve

    #Sets the getinv() call to be the inverse of the matrix as specified in above funtion
        getinv <- function() im


    #Sets the matrixs to have newly defined functions - as specified in code above
        list(set = set, get = get, setinv = setinv, getinv = getinv) }




#Function to calculate the inverse of a matrix if it hasn't been calculated and store the inverse into cache, or #Pull back previously calculated inverse of a matrix from cache

cacheSolve <- function(x, ...) {
im <- x$getinv()


    #If statement checks if inverse of exists in cache, if so - returns the cached inverse matrix
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }

    #Fall back (else - inverse of matrix is not cached), hence undertakes calculation of inverse of matrix
        data <- x$get()
    im <- solve(data, ...)

    #Stores calculed matrix inverse to cache
        x$setinv(im)

    #Prints matrix inverse
    im
}


# Testing Script
# x <- matrix(rnorm(25), nrow = 5)
# A <- makeCacheMatrix(x)
# A$get()
# A$getinv()
# cacheSolve(A)
# cacheSolve(A)