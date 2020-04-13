#Gram-Schmidt orthogonalization algorithm

proj <- function(u,v) {   #definition of the projection operator
x <- c((v %*% u)/(u %*% u)) * u
}

V <- matrix(c(5,9,2,1,-7,3,2,4,1),3,3) #3x3 matrix definition
print('Input matrix: ', quote=FALSE)
print(V)

v1 <- V[,1]
v2 <- V[,2]
v3 <- V[,3] #vectors definition

U <- matrix(,3,3)  # definition of the empty matrix to hold the results

#Orthogonalisation
u1 <- v1      #assign first column of M to first column of V
u2 <- v2 - proj(u1, v2) #fill second column of V with the result of the projection operator on vectors u1 and v2 substracted from v2
u3 <- v3 - proj(u1, v3) - proj(u2, v3)

U <- cbind(u1,u2,u3) #creates orthogonal matrix, where dot product of ui, uj is equal to 0

print('Ortogonality check:', quote = FALSE)
u1 %*% u2

print('Orthogonal matrix: ', quote=FALSE)
U

#Orthonormalisation
u1_n <- u1/sqrt(c(u1 %*% u1))
u2_n <- u2/sqrt(c(u2 %*% u2))
u3_n <- u3/sqrt(c(u3 %*% u3))

print('Orthonormal matrix: ')
U_n <- cbind(u1_n,u2_n,u3_n)
U_n

#N dimension generalisation
N <- 100
M <- replicate(N, rnorm(N)) #creates random matrix of size NxN. replicate function replicates values from rnorm N times. rnorm generates a vector of normally distributed random numbers.
print('V1.0 - random matrix of size N')
M
X <- matrix(, N, N) #creates empty matrix of size NxN for results

#GS process for NxN matrix
X[,1] <- M[,1] #initalise first column

for (i in 2 : N) {
  X[,i] <- M[,i]
  for (j in 1 : (i-1)) {
    X[,i] <- X[,i] - proj(X[,j], M[,i])
  }
}

print('Orthogonalised matrix:', quote = FALSE)
X

X[,2]%*%X[,3]




