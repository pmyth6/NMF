load("zipCodeAllDigits.RData")
library(NMF)
col = hcl.colors(1000, "blue-red", rev = FALSE)

flip_horizontal <- function(img_matrix) {
  flipped_matrix <- img_matrix[, ncol(img_matrix):1]
  return(flipped_matrix)
}

x = train.X
tx = t(train.X)
tx = (tx+1)/2
x = (x+1)/2

print.x=matrix(tx[,38], 16, 16)
print.x = flip_horizontal(print.x)
par(mfrow = c(1,1), mar = c(1, 1, 1, 1))
image(print.x, axes=FALSE, col=hcl.colors(1000, "blue-red", rev = FALSE))

result = prcomp(x, center =TRUE, rank.=4)
par(mfrow = c(1,4), mar = c(0.1, 0.1, 0.1, 0.1), bg="black")
for (i in 1:4){
  print.x.approx = matrix(result$rotation[,i], 16, 16)
  print.x.approx = flip_horizontal(print.x.approx)
  pos = print.x.approx
  pos[pos<0] = NA
  neg = print.x.approx
  neg[neg>0] = NA
  image(pos, axes=FALSE, add=FALSE) 
  image(neg, axes=FALSE, add=TRUE, col=neg_cols)
}


result = prcomp(x, center =TRUE, rank.=10)
encoded = result$x %*% t(result$rotation)
par(mfrow = c(5,10), mar = c(0.1, 0.1, 0.1, 0.1), bg="white")
for (i in 1:25){
  print.eg = matrix(encoded[i,], 16, 16)
  print.eg = flip_horizontal(print.eg)
  #pos = print.eg
  #pos[pos<0] = NA
  #neg = print.eg
  #neg[neg>0] = NA
  image(print.eg, axes=FALSE, add=FALSE, col = hcl.colors(1000, "blue-red", rev = FALSE)) #, col=pos_cols)
  #image(neg, axes=FALSE, add=TRUE, col=neg_cols)
  print.x=matrix(x[,i], 16, 16)
  print.x = flip_horizontal(print.x)
  image(print.x, axes=FALSE) #, col=pos_cols)
}



