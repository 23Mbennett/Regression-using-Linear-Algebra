## Linear Regression Using Matrices!!

#Get 15 observations of data before starting, I used Real Median Household Incomes 2001-2015 (as Y's), my data is attached for use

#Get vector of Data
Ydata <- (MATRIX_REGRESSION)[,1]

#Remove the NA's from your data
Y <- na.omit(Ydata)

#Change class of table to matrix or array so you perform linear algebra
Y <- as.matrix(Y)
class(Y)
dim(Y)

#I generated a Time variable but you could have just read in another X variable
Time <- array(sequence(15), dim = c(15,1))

# for this model the X variable matrix must have 15 rows and 2 columns as I used 15 obs and want to estimate B0 + B1
## Generated vector/array of 1's
B <- array(rep(1), dim = c(15,1))

###make matrix X by combining two previous vectors
X <- cbind(B, Time)

##################  Formula: B's = ((X'X)^-1)X'Y  ########################

#Define X'
X_transpose <- t(X)
 
#X'X
XprimeX <- X_transpose %*% X

#(X'X)^-1
invXprimeX <- solve(XprimeX)

#((X'X)^-1))X'
invXprimeX_Xprime <- inv_XprimeX %*% t(X)

#((X'X)^-1)X'Y
invXprimeX_Xprime_Y <- invXprimeX_Xprime %*% Y

#print: B's = ((X'X)^-1)X'Y

invXprimeX_Xprime_Y
#Y
#[1,] 56658.3905
#[2,]  -180.0321



##lets double check to see if we estimated the correct (B's) parameters lm() command
lm(Y ~ Time)
#Coefficients:
#(Intercept)         Time  
#56658         -180