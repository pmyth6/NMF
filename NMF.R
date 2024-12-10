load("zipCodeAllDigits.RData")
library(NMF)

flip_horizontal <- function(img_matrix) {
  flipped_matrix <- img_matrix[, ncol(img_matrix):1]
  return(flipped_matrix)
}

x = t(train.X)

print.x=matrix(x[,38], 16, 16)
print.x = flip_horizontal(print.x)
par(mfrow = c(1,1), mar = c(1, 1, 1, 1))
image(print.x, axes=FALSE)

result = nmf((x+1)/2, 10)

par(mfrow = c(2, 5), mar = c(0.1, 0.1, 0.1, 0.1), bg="black")

for (i in 1:10){
  print.w.1 = matrix(result@fit@W[,i], 16, 16)
  print.w.1 = flip_horizontal(print.w.1)
  
  image(print.w.1, axes=FALSE)
}

approx = result@fit@W %*% result@fit@H[, 38]
p.approx = matrix(approx, 16, 16)
p.approx = flip_horizontal(p.approx)
par(mfrow = c(1,1), mar = c(1, 1, 1, 1))
image(p.approx, axes=FALSE)

