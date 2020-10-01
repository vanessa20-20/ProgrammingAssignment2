makeCacheMatrix<- function (x= matrix()) {
  ## create a matrix object x and some associated sub-functions/methods
  ## define the cache m
  m<-NULL
  set<-function(y){
    x<<-y ## assign the input matrix y to the variable x 
    m<<-NULL ## re-initialize m in the parent environment to null
  }
  get<-function() x ## return the matrix x
  setinverse= function (inverse)  m<<- inverse ## set the cache m equal to the inverse of the matrix x
  getinverse = function () m  ## return the cached inverse of x
  list (set=set, get=get, setinverse=setinverse, 
        getinverse=getinverse) 
}

## The following function calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse
## has already been caclulated. If so, it 'get's the inverse from the cache
## and skips the computation. Otherwise, it calculates the matrix inverse
## and sets the value of the inverse in the cache via the 'setinverse' function.
cacheSolve<- function (x) {  ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)) {
    message ("gettin cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data)
  x$setinverse(m)
  m
}

#to try the functions
my_matrix<-matrix(c(1,2,3,4,2,5,6,7,3,6,8,9,4,7,9,10), nrow=4,ncol=4, byrow =TRUE)
z<-makeCacheMatrix(my_matrix)
cacheSolve(z)
