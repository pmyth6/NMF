# Do pca and nmf on numbers data set 
# Evaluate as basis and encodings
# Create plots
r <- 200
# Importing the data ----
load("zipCodeAllDigits.RData")
X <- (rbind(train.X, test.X)+1)/2 # (rbind(train.X, test.X)+1)/2
y <- c(train.y, test.y)
X |> dim()
y |> length()

# function to plot the image data
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



# PCA ----
X_pca <- prcomp(X) 
# rot gives the loadings (basis)
# x gives the scores (encoding)
B <- X_pca$rotation ; dim(B)
e <- X_pca$x ; dim(e)


# Plot grid of images with their pca approx ----
n <- 5; m <- 5; N <- n*m  # n is points plotted
skip <- 0       # skip is how far into data to start plots
# on xaxis (are pairs so will be twice n), m is num of plots on yax 
par(mfrow = c(m, n*2)); par(mar = c(0.1, 0.1, 0.1, 0.1), bg="white")
sc <- sapply(1:N+skip, \(i) max(B[, 1:r] %*% e[i, 1:r]) ) |> max()
for (i in 1:N) {
  plot_dig(B[, 1:r] %*% e[i+skip, 1:r])
  plot_dig(X[i + skip, ])
}

# Plot of pca basis (loadings) from previous chunk ----
n <- ceiling(r^(1/2))  # n is points plotted
# on xaxis (are pairs so will be twice n), m is num of plots on yax 
par(mfrow = c(20, 10)); par(mar = c(0.1, 0.1, 0.1, 0.1), bg="black")
sc <- max(B[,1:r])
for (i in 1:r) {
  plot_dig(B[, i], scale = NULL)
}

# NMF ----
X_nmf <- nmf(t(X), r, "Frobenius")
# W gives the loadings (basis)
# H gives the scores (encoding)
W <- X_nmf@fit@W ; dim(W)
H <- X_nmf@fit@H ; dim(H)

# Plot grid of images with their nmf approx ----
n <- 5; m <- 5; N <- n*m  # n is points plotted
skip <- 0       # skip is how far into data to start plots
# on xaxis (are pairs so will be twice n), m is num of plots on yax 
par(mfrow = c(m, n*2)); par(mar = c(0.1, 0.1, 0.1, 0.1), bg="white")
sc <- sapply(1:N+skip, \(i) max(W[, 1:r] %*% H[1:r, i]) ) |> max()
for (i in 1:N) {
  plot_dig(W[, 1:r] %*% H[1:r, i+skip])
  plot_dig(X[i + skip, ])
}

# Plot of nmf basis (loadings) from previous chunk ----
n <- ceiling(r^(1/2))  # n is points plotted
# on xaxis (are pairs so will be twice n), m is num of plots on yax 
par(mfrow = c(20, 10)); par(mar = c(0.1, 0.1, 0.1, 0.1), bg="black")
sc <- max(W[,1:r])
for (i in 1:r) {
  plot_dig(W[, i], scale = NULL)
}
