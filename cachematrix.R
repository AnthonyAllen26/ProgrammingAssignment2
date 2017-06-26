## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


##This function will take an input value which we are given as makeCacheMatrix.
##The function will then cycle through setting the value of the matrix, getting the valiue of the matrix,
##setting the value of the inverse, and getting the value of the inverse
##this is done by cacheing it's own inverse

makeCacheMatrix <- function(x = matrix()) { 
	##This function creates a special "matrix" object that can cache its inverse.
	
	antMat <-NULL ##iniitialiize matrix starting point value
	setMat <-function(y) {
		##define the set function with the 'y' variable
		
		x <<- y
		antMat <<- NULL
		
	}

getMatrix <- function() x  ##This will get the value of the matrix
            setinv <- function(inverse) antMat <<- inverse ##set the invertible Matrix
            getinv <- function() antMat ##Get the value of the invertible matrix
            list(setMat = setMat, getMat = getMat,
                 setinv = setinv,
                 getinv = getinv)
}


## Write a short comment describing this function

##This function will retrieve the value that was calculated in the previous function
##and compute the inverse. If this is done already then it will retrieve the 
##inverse from the cache

cacheSolve <- function(x, ...) {
        
##Get the matrix caluclated from the makeCacheMatrix function
            antMat <- x$getinv()
            
##If statement to get the matrix if it is not Null
            if(!is.null(antMat)) {
                    message("Getting invertible matrix") ##Display message to user
                    return(antMat)    ##return the invertible matric
            }
            
##If statement to create the Matrix if the values from MakeCacheMatrix is NULL

            Matdata <- x$getMat()
            antMat <- solve(Matdata, ...) ##The solve function is used to invert the square matrix
            x$setinv(antMat) ##set the invertible matrix value
            antMat ##Displays the inverted matrix
}
