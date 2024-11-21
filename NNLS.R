load("zipCode138.RData")
#set data to only 3s and 8s
y = y[31:90] 

#convert data to positive between 0 and 1
X = (train.X[31:90,]+1)/2 
X.array = (train.X.array[, , 31:90]+1)/2

#split data again into 3s and 8s
X3 = X[1:30,]
X8 = X[31:60,]

X.array3 = X.array[, , 1:30]
X.array8 = X.array[, , 31:60]

#create average images
W3 = matrix(0, 16, 16)
for (i in 1:30){
  W3 = W3+X.array3[, , i]
}
W3 = W3/30

image(W3, axes = FALSE)

W8 = matrix(0, 16, 16)
for (i in 1:30){
  W8 = W8+X.array8[, , i]
}
W8 = W8/30

image(W8)

#Rotate the data for the 3s so that it lines up with the 8s more closely
rotate_180 = function(matrix) {
  matrix <- matrix[nrow(matrix):1, ]
  matrix <- matrix[, ncol(matrix):1]
  return(matrix)
}

W3 = rotate_180(W3)
image(W3)

W.combine = (W8 + W3)/2
image(W.combine, axes = FALSE)

W.difference = abs(W8 - W3)
image(W8, axes = FALSE)
image(W3, axes = FALSE)
image(W.difference, axes = FALSE)

par(mfrow = c(2, 2), mar = c(3, 3, 3, 3))
image(W8, main = "Average of the 8s", axes = FALSE)
image(W3, main = "Average of the 3s, basis 1", axes = FALSE)
image(W.combine, main = "Average of the 8s and 3s", axes = FALSE)
image(W.difference, main = "Average of the 8s minus the 3s, basis 2", axes = FALSE)

#for W we need to resize the image matrices so they're both 16**2 in length,
#and then bind them together as a 16**2 by 2 matrix

W.basis.1 = as.vector(W3)
W.basis.2 = as.vector(W.difference)

W = cbind(W.basis.1, W.basis.2)

#do the same for the data (do this later) 

#Wh = x
par(mfrow = c(1, 1))
x = rotate_180(X.array3[, , 1])
image(x, axes=FALSE) #printing x as an image
x = as.vector(x)

h = NNLS(W, x, 1.0e-7)

#print Wh as an image
Wh = W%*%h
Wh = matrix(Wh, 16, 16)
image(Wh, axes=FALSE)

#Try the same for an 8
#Wh = x
par(mfrow = c(1, 1))
x = X.array8[, , 1]
image(x, axes=FALSE) #printing x as an image
x = as.vector(x)

h = NNLS(W, x, 1.0e-7)

#print Wh as an image
Wh = W%*%h
Wh = matrix(Wh, 16, 16)
image(Wh, axes=FALSE)

par(mfrow = c(1, 2), mar = c(0.5, 0.5, 0, 0.5))
image(W3, axes=FALSE)
image(W.difference, axes=FALSE)

