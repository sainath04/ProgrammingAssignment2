## Consists of 2 functions namely, makeCacheMatrix and cacheSolve
## makeCacheMatrix reads the matrix and its inverse and store them in a list.
## cacheSolve checks for the inverse of given matrix. If the inverse is already calculated
## it returns the value. Else the inverse will be calculated and is returned to the list in makeCacheMatrix.



## Create a cache function to store matrix and its inverse

makeCacheMatrix <- function(x=matrix())
{
    inv<-NULL
    set <- function(y)
    {
        x <<- y
        inv<-NULL
    }
    get <- function()x
    setInv <- function(inverse)
    {
        inv<<-inverse
    }
    getInv<-function()inv
    list(set=set, get=get, setinv=setInv, getinv=getInv)
    
}


## Create a function to check for inverse of the given function and calculate if not already available.(return stored value if already calculated)

cachesolve <- function(x,...)
{
    inv <- x$getinv()
    if(!is.null(inv))
    {
        message("Getting Cached data")
        return(inv)
    }
    Data<-x$get()
    inv<-solve(Data)
    x$setinv(inv)
    inv
}


#Define matrix

mat<-matrix(1:4,2,2)

# Run functions on the given matrix
a <- makeCacheMatrix(mat)
cachesolve(a)