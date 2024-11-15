library(pixmap)

# Reproducing L-S (1999) face analysis----
N <- 2429
V <- matrix(NA, nc = N, nr = 19*19)

for (i in 1:N) {
  a <- '00000'
  bc <- as.character(i)
  substr(a, 5-(floor(log10(i))), 5-0) <- bc
  loc <- paste("faces/face.train/train/face/", "face", a, ".pgm", sep = '')
  
  im <- read.pnm(loc)
  v_im <- as.vector(im@grey)
  v_im <- v_im * 0.5 / var(v_im)^(0.5) + 0.25 - mean(v_im)* 0.5 / var(v_im)^(0.5)
  v_im[v_im < 0] <- 0; v_im[v_im > 1] <- 1 
  V[,i] <- v_im
}

# Method used by L-S using random initial matrix
# 500 iterations is sufficient for convergence (tho an error could be used)
set.seed(123)
r <- 49 # THE NUMBER OF BASIS IMAGES
H <- matrix(runif(r*N), nr = r, nc = N) 
W <- matrix(runif(19*19*r), nr = 19*19, nc = r) 
for (i in 1:500) {
  Hn <- H * (t(W) %*% V) / (t(W) %*% W %*% H)
  Wn <- W * (V %*% t(H)) / (W %*% H %*% t(H))
  H <- Hn
  W <- Wn
}
max(W)

# Plot NMF basis images
# This plots individual basis'
Wpix <- pixmapGrey(W[, 1], nrow = 19, nc = 19)
Wpix <- pixmapGrey(1 - Wpix@grey, nrow = 19, nc = 19)
plot(Wpix)



# Plot all 49 basis images as a square grid
for (i in 1:7) {
  for (j in 1:7) {
    # Note that W elements can exceed 1. Pixmap type will clip W into [0,1] for us.
    Wpix <- pixmapGrey(W[, i + (j-1)*7], nrow = 19, nc = 19)
    # Invert the grey scale (just 1 - image)
    Wpix <- pixmapGrey(1 - Wpix@grey, nrow = 19, nc = 19, bbox=c(i-1,(j-1),(i-1)+1,(j-1)+1))
    par(new = TRUE)
    if (i == 1 & j == 1){par(new = FALSE)}
    plot(Wpix, ylim = c(0, 7+0.01), xlim = c(1,6))
  }
}
# Plot lines to seperate them
for (i in 1:8) {
  lines(c(0, 7), c(i-1, i-1), col = "steelblue", lwd = 2)
  lines(c(i-1, i-1), c(0, 7), col = "steelblue", lwd = 2)
}


# Reading and plotting .pgm files----
im <- read.pnm("faces/face.train/train/face/face00001.pgm") # read in image
plot(im) # can plot the image

mat <- im@grey # can turn into a data matrix for implimenting methods
am_2 <- pixmapGrey(mat) # can recover fancy class
plot(am_2)  # so that we can nicely plot the out put of our algorithims
