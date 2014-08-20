## This program consists of two functions "makeCacheMatrix" and "cacheSolve"
## which calculate the inverse of a matrix and stores its value similar to a 
## cache and so that we can lookup the result without recalculating the inverse.
## This is accomplished in two steps.

## Step 1:

## The makeCacheMatrix function accepts a matrix as its input and generates a list
## which contains the user defined functions set, get setinv and getinv.  The output
## of this function is a list object that needs to be assigned to a user defined variable.
## The main feature of this function is the "<<-" assignment operator. variable assignments 
## and values are local in the function definition and are lost once you exit the function.
## The <<- operator is a global assignmnet operator and the variable values are not lost once
## we exit the function.
##
## e.g. k <- makeCacheMatrix(x) 
## where x is a matrix whose inverse needs to be calculated

makeCacheMatrix <- function(x = matrix()) {
	 minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) minv <<- inverse
        getinv <- function() minv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Step 2:

## The cacheSolve function calculates the inverse of the matrix. It first performs a check 
## to see of the minv of the given matrix has already been calculated and if so (minv in not NULL)
## returns the minv. If the matrix inverse needs to calculated then it uses the solve function to 
## calculate the inverse and set the inverse to minv.
## usage, e.g. cacheSolve(k)
## where k is the list object from step 1.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		 minv <- x$getinv()
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
		}
        data <- x$get()
        minv <- solve(data, ...)
        x$setinv(minv)
        minv
}
