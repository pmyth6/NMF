library(NMF)
r <- 25
# Importing the data ----
load("zipCodeAllDigits.RData")
X <- (rbind(train.X, test.X)+1)/2
y <- c(train.y, test.y)
# X |> dim()
# y |> length()

# Simple plot of image vector ----
# plot(X[1, ])
# tes <- X[1, ]
# tes[20:30] <- -0.01
# image(matrix(tes, 16, 16)[, 16:1], zlim = c(-1,1),
#       col = hcl.colors(1000, "blue-red"))
# Now turn this into a function
plot_dig <- function(x, scale = NULL, rev = FALSE, axes=FALSE) {
  # Assume x is a real vector length 16*16
  if (is.null(scale)){
    l <- max(x, -x)
  }else {
    l <- scale
  }
  x <- matrix(x, 16, 16)[, 16:1] 
  image(x, zlim = c(-l, l), axes=FALSE,
        col = hcl.colors(1000, "blue-red", rev = rev))
}
# plot_dig(X[5,])


# NMF ----
#?nmf

X_nmf <- nmf(t(X), r, "Lee")
B <- X_nmf@fit@W
e <- X_nmf@fit@H

# simple image plots ----
# n <- 50
# x_tes <- B %*% e[, n]
# plot_dig(x_tes)
# plot_dig(X[n, ])

# Plot grid of images ----
n <- 3; m <- 4; N <- n*m  # n is points plotted
skip <- 10       # skip is how far into data to start plots
# on xaxis (are pairs so will be twice n), m is num of plots on yax 
par(mfrow = c(m, n*2)); par(mar = c(0.5, 0.5, 0, 0.5))
sc <- sapply(1:N+skip, \(i) max(B[, 1:r] %*% e[i, 1:r]) ) |> max()
for (i in 1:N) {
  plot_dig(B[, 1:r] %*% e[, i+skip])
  plot_dig(X[i + skip, ])
}
par(mfrow = c(1,1)) # Return to default par
par(mar = c(5, 4, 4, 2) + 0.1)

# Plot of pca basis from previous chunk ----
n <- ceiling(r^(1/2))  # n is points plotted
# on xaxis (are pairs so will be twice n), m is num of plots on yax 
par(mfrow = c(n, n)); par(mar = c(0.5, 0.5, 0, 0.5))
sc <- max(B[,1:r])
for (i in 1:r) {
  plot_dig(B[, i], scale = NULL)
}
par(mfrow = c(1,1)) # Return to default par
par(mar = c(5, 4, 4, 2) + 0.1)