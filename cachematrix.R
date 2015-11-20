##This function will take the inverse of a matrix and then cache it as the computation will take long time to calculate.

##In this function we have used <<- operator which is used to assign the matrix value to an object into a object 
##that is different from current environment.
##Below function will create a matrix "MakeCacheMatrix" to set the value of the metrix
##Set Inverse of the martix and GET Inverse of the matrix


makeCacheMatrix <-function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the MakeCacheMatrix
## If the inverse has already been calculated and the matrix is already created the it will return to NULL by skipping the computation otherwise
## it will inverse the Matrix and stores the value in the cache via the Setsolve function.



cacheSolve <- function(x, ...) {
        ## Rget inverse of a metrix
        m <- x$getsolve()
        # Returns the inverse if the inverse is already calculated
        if(!is.null(m)) {
        message("getting cached data")
        return(m)
        }
        #computes inverses of a matrix if it is a new matrix
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

y = matrix(1:4,2,2)
y

a<-makeCacheMatrix(y)
a

cacheSolve(a)
