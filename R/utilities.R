# Returns a 2x51 matrix representing the vertices of a
# polygon with approximates the ellipse formed by 
# applying the 2x2 matrix, M, to a circle of radius
# r which is centered at the origin.
# If result <- ellipse(r, M), use
# polygon(x+result[1,], y+result[2,]) to plot the
# the ellipse centered at x, y.
ellipse <- function(r, M=matrix(c(1,0,0,1), 2, 2)){
  theta <- seq(0, 2*pi, length.out = 51)
  temp <- r*cbind(cos(theta), sin(theta)) %*% M
  colnames(temp) <- c("x", "y")
  return(temp)
}

translate <- function(figure, vect){
  t(apply(figure, 1, function(x)x+vect))
}

# compute margins which make plot window square
adj_margins <- function(){
  fin <- par("fin") # (window in inches)
  mai <- par("mai") # (margins in inches)
  # We want fin[1]-(mai[2]+mai[4]) == fin[2] - (mai[1]+mai[3])
  w <- fin[1]-(mai[2]+mai[4]) # plot window width
  h <- fin[2]-(mai[1]+mai[3]) # plot window height
  # We'll preserve the smallest of these two dimensions
  if(w > h){
    delta <- (w-h)/2
    mai[c(2,4)] <- mai[c(2,4)] + delta
  } else {
    delta <- (h-w)/2
    mai[c(1,3)] <- mai[c(1,3)] + delta
  }
  mai
}

# Plot a square grid in a square plot window with grid lines
# given by the tix argument, resetting par("mai") to its
# original value after the fact.
makegrid <- function(tix, xlab="x", ylab="y", major_axes=TRUE, ...){
  mai <- par("mai")
  par(mai=adj_margins())
  plot(tix, tix, type='n', xlab=xlab, ylab=ylab, ...)
  abline(h=tix, lwd=2, col="lightblue")
  abline(v=tix, lwd=2, col="lightblue")
  if(major_axes){
    abline(v=0)
    abline(h=0)
  }
  par(mai=mai)
  invisible()
}
