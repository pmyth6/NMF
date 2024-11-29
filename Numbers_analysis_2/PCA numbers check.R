# Do pca on numbers data set 
# Evaluate as basis and encodings
# Create plots
r <- 25
# Importing the data ----
load("zipCodeAllDigits.RData")
X <- (rbind(train.X, test.X)+1)/2 # (rbind(train.X, test.X)+1)/2
y <- c(train.y, test.y)
X |> dim()
y |> length()
#X <- data.frame(X)
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


# PCA ----
X_pca <- prcomp(X) #easy
# rot gives the loadings (basis)
# x gives the scores (encoding)
B <- X_pca$rotation ; dim(B)
e <- X_pca$x ; dim(e)


# Simple image plots ----
# a point is then x_tes
# n <- 11
# r <- 200
# plot_dig((e[,1:r] %*% t(B[,1:r]))[,11])
# x_tes <- e[, 1:20] %*% t(B[, 1:20])
# plot_dig(x_tes[, 9])
# plot_dig(X[, n])
# recon =  e[,1:r] %*% t(B[,1:r])
# plot_dig(recon[1, ])
# 
# # to approximate we truncate
# n <- 11
# x_tes <- B[, 1:r] %*% e[, 1:r]
# plot_dig(x_tes[, 2])
# plot_dig(X[n, ])

# Plot grid of images with their pca approx ----
n <- 3; m <- 4; N <- n*m  # n is points plotted
skip <- 10       # skip is how far into data to start plots
# on xaxis (are pairs so will be twice n), m is num of plots on yax 
par(mfrow = c(m, n*2)); par(mar = c(0.5, 0.5, 0, 0.5))
sc <- sapply(1:N+skip, \(i) max(B[, 1:r] %*% e[i, 1:r]) ) |> max()
for (i in 1:N) {
  plot_dig(B[, 1:r] %*% e[i+skip, 1:r])
  plot_dig(X[i + skip, ])
}
par(mfrow = c(1,1)) # Return to default par
par(mar = c(5, 4, 4, 2) + 0.1)

# Plot of pca basis (loadings) from previous chunk ----
n <- ceiling(r^(1/2))  # n is points plotted
# on xaxis (are pairs so will be twice n), m is num of plots on yax 
par(mfrow = c(n, n)); par(mar = c(0.5, 0.5, 0, 0.5))
sc <- max(B[,1:r])
for (i in 1:r) {
  plot_dig(B[, i], scale = NULL)
}
par(mfrow = c(1,1)) # Return to default par
par(mar = c(5, 4, 4, 2) + 0.1)

# scree plot ----
plot(X_pca, type = 'l', npcs = 200) # default scree plot
plot(1:200, X_pca$sdev[1:200]^2 / sum(X_pca$sdev^2), 'b')
plot(1:200, cumsum(X_pca$sdev[1:200]^2 / sum(X_pca$sdev^2)), 'b',
     ylab = "Cumulative accounted variance", xlab = "Number of principle components")
abline(h = c(0.95, 0.9), v = c(49, 75))
text(x = 45, y = 0.3, "49")
text(x = 70, y = 0.3, "75")
