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

par(mfrow = c(1,1), mar = c(0, 0, 0, 0))
plot_dig(W3)

W8 = matrix(0, 16, 16)
for (i in 1:30){
  W8 = W8+X.array8[, , i]
}
W8 = W8/30

par(mfrow = c(1,1), mar = c(0, 0, 0, 0))
plot_dig(W8)

#Rotate the data for the 3s so that it lines up with the 8s more closely
rotate_180 = function(matrix) {
  matrix <- matrix[nrow(matrix):1, ]
  matrix <- matrix[, ncol(matrix):1]
  return(matrix)
}

W3 = rotate_180(W3)
par(mfrow = c(1,1), mar = c(0, 0, 0, 0))
plot_dig(W3)

W.combine = (W8 + W3)/2
par(mfrow = c(1,1), mar = c(0, 0, 0, 0))
plot_dig(W.combine)

W.difference = abs(W8 - W3)
image(W8, axes = FALSE)
image(W3, axes = FALSE)
par(mfrow = c(1,1), mar = c(0, 0, 0, 0))
plot_dig(W.difference)

#for W we need to resize the image matrices so they're both 16**2 in length,
#and then bind them together as a 16**2 by 2 matrix

W.basis.1 = as.vector(W3)
W.basis.2 = as.vector(W.difference)

W = cbind(W.basis.1, W.basis.2)

#do the same for the data (do this later) 

#Wh = x
par(mfrow = c(1,1), mar = c(0, 0, 0, 0))
x = rotate_180(X.array3[, , 1])
plot_dig(x) #printing x as an image
x = as.vector(x)

h = NNLS(W, x, 1.0e-7)

#print Wh as an image
Wh = W%*%h
Wh = matrix(Wh, 16, 16)
par(mfrow = c(1,1), mar = c(0, 0, 0, 0))
plot_dig(Wh)

#Try the same for an 8
#Wh = x
par(mfrow = c(1, 1))
x = X.array8[, , 1]
par(mfrow = c(1,1), mar = c(0, 0, 0, 0))
plot_dig(x) #printing x as an image
x = as.vector(x)

h = NNLS(W, x, 1.0e-7)

#print Wh as an image
Wh = W%*%h
Wh = matrix(Wh, 16, 16)
par(mfrow = c(1,1), mar = c(0, 0, 0, 0))
plot_dig(Wh)

par(mfrow = c(1, 2), mar = c(0.1, 0.1, 0.1, 0.1), bg="black")
plot_dig(W3)
plot_dig(W.difference)

