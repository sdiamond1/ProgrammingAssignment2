## The following two functions are used to cache the inverse of a matrix and
## recompute the inverse if it is different from the chached value

## this is a matrix to use as an example, just un-comment and run to store
## a <- matrix(rnorm(100),10,10)

## The following function "makeCacheMatrix" creates a special "matrix" that is
## associated with 4 functions and an environment

makeCacheMatrix <- function(x = matrix()) {
    
    m<-NULL ## this variable will store the cached matrix
    
    set<-function(y){ ## function used to set new matrix, and reset m = NULL
        x<<-y
        m<<-NULL
    }
    get<-function(){ ## function used to query current matrix (x)
        x
    }
    setmatrix<-function(solve){  ## function used to store values to m
        m<<- solve
    }
    getmatrix<-function(){ ## function used to query current value of m
        m
    }
    
    list(set=set, get=get, ## outputs list object of all funcitons
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}

## The function "cacheSolve" takes the output of "makeCacheMatrix" and either
## computes and returns the inverse matrix or returnes the cached inverse matrix

cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix() ## first querys value of m
    
    if(!is.null(m)){ ## !=not so if m is not NULL then it returns m
        message("getting cached data")
        return(m)
    }
    matrix<-x$get() ## if m=NULL then x is queried and stored to matrix
    m<-solve(matrix, ...) ## solve computes inverse of matrix and stores to m
    x$setmatrix(m) 
    m
}
