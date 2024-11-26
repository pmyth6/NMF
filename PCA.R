load("zipCodeAllDigits.RData")
library(NMF)
neg_cols = colorRampPalette(c("#FFFFC8", "blue"))(100)

flip_horizontal <- function(img_matrix) {
  flipped_matrix <- img_matrix[, ncol(img_matrix):1]
  return(flipped_matrix)
}

x = t(train.X)
x = (x+1)/2

print.x=matrix(x[,38], 16, 16)
print.x = flip_horizontal(print.x)
par(mfrow = c(1,1), mar = c(1, 1, 1, 1))
image(print.x, axes=FALSE)

result = prcomp(x, center =TRUE, rank.=25)
print.x.approx = matrix(result$x[,4], 16, 16)
print.x.approx = flip_horizontal(print.x.approx)
par(mfrow = c(1,1), mar = c(1, 1, 1, 1))
image(print.x.approx, axes=FALSE)

result = prcomp(x, center =TRUE, rank.=20)
encoded = result$x %*% t(result$rotation)
par(mfrow = c(5,10), mar = c(0.1, 0.1, 0.1, 0.1))
for (i in 1:25){
  print.eg = matrix(encoded[,i], 16, 16)
  print.eg = flip_horizontal(print.eg)
  pos = print.eg
  pos[pos<0] = NA
  neg = print.eg
  neg[neg>0] = NA
  image(pos, axes=FALSE, add=FALSE) #, col=pos_cols)
  image(neg, axes=FALSE, add=TRUE, col=neg_cols)
  print.x=matrix(x[,i], 16, 16)
  print.x = flip_horizontal(print.x)
  image(print.x, axes=FALSE) #, col=pos_cols)
}



