## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## INVERSE MATRIX (Return a matrix that is the inverse of 'x')

makeCacheMatrix <- function(x = matrix()) {
        # setting solve to NULL
        mm <- NULL
        # New value being assigned to x
        set <- function(y) { 
                x <<- y
                #Setting solve as NULL
                mm <<- NULL 
        }
        #Get function will return object
        get <- function() x 
        #Mean function set, cache solve
        setsolve <- function(solve) mm <<- solve 
        #Get object function, return object
        getsolve <- function() mm 
        list(set = set, get = get, 
             #list of objects
             setsolve = setsolve,
             getsolve = getsolve)
}

## Write a short comment describing this function

#Object x calling get solve
cachesolve <- function(x, ...) { 
        mm <- x$getsolve()
        #Check if cache solve present, and then return cached solve
        if(!is.null(mm)){ 
                return(mm)
        }
        #find solve of data
        data <- x$get() 
        #call set solve
        mm <- solve(data, ...) 
        #return solve
        x$setsolve(mm) 
        mm
}

#Test
#matr1<-matrix(sample.int(10000,size=1089,replace=TRUE), nrow=33)
#bigvector<- makeCacheMatrix(matr1) 
#cachesolve(bigvector)   
#solve(matr1)
#matr1
