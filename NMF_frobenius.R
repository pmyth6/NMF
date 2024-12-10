
r = 20

load("zipCodeAllDigits.RData")
library(NMF)

flip_horizontal <- function(img_matrix) {
  flipped_matrix <- img_matrix[, ncol(img_matrix):1]
  return(flipped_matrix)
}

x = t(train.X)
x = (x+1)/2
mean(x)

print.x=matrix(x[,38], 16, 16)
print.x = flip_horizontal(print.x)
par(mfrow = c(1,1), mar = c(1, 1, 1, 1))
image(print.x, axes=FALSE)


result = nmf(x, r, "Frobenius")

par(mfrow = c(2, 10), mar = c(0.1, 0.1, 0.1, 0.1), bg="black")
for (i in 1:r){
  print.w.1 = matrix(result@fit@W[,i], 16, 16)
  print.w.1 = flip_horizontal(print.w.1)
  
  image(print.w.1, axes=FALSE)
}

par(mfrow = c(5,10), mar = c(0.1, 0.1, 0.1, 0.1), bg="white")
for (i in 1:25){
  approx = result@fit@W %*% result@fit@H[, i]
  p.approx = matrix(approx, 16, 16)
  p.approx = flip_horizontal(p.approx)
  image(p.approx, axes=FALSE)
  image(flip_horizontal(matrix(x[,i], 16, 16)), axes=FALSE)
}





par(mfrow = c(1,2), mar = c(1, 1, 1, 1), bg="white")
for (i in 11:11){
  approx = result@fit@W %*% result@fit@H[, i]
  p.approx = matrix(approx, 16, 16)
  p.approx = flip_horizontal(p.approx)
  image(p.approx, axes=FALSE)
  image(flip_horizontal(matrix(x[,i], 16, 16)), axes=FALSE)
}


