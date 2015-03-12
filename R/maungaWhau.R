# Utilities for plotting and taking gradients of the Maunga Whau data.
# Adapted from demo(persp)


z <- 2*datasets::volcano # 2* to exagerate relief
x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)
z0 <- min(z) - 20
z <- rbind(z0, cbind(z0, z, z0), z0)
x <- c(min(x) - 1e-10, x, max(x) + 1e-10)
y <- c(min(y) - 1e-10, y, max(y) + 1e-10)
fill <- matrix("green3", nrow = nrow(z)-1, ncol = ncol(z)-1)
fill[ , i2 <- c(1,ncol(fill))] <- "gray"
fill[i1 <- c(1,nrow(fill)) , ] <- "gray"

local({
  # Using interpolation, make a function which gives altitude as a function of x, y.
  makemw <- function(x,y,z){
    function(vec){
      sn <- vec[1]
      ew <- vec[2]
      if(isTRUE(sn < min(x) | sn > max(x) | ew < min(y) | ew > max(y)))return(c(NA,NA))
      # indices of the x and y coordinates just less than sn and ew, resp
      i1 <- sum(x <= sn)
      j1 <- sum(y <= ew)
      if(isTRUE(i1 >= length(x) | j1 >= length(y)))return(c(NA, NA))
      if(is.na(i1) | is.na(j1))return(vec)
      if(isTRUE(all.equal(vec, c(x[i1], x[j1]))))return(z[i1, j1])
      # Small box around these coordinates
      i <- max(1, i1):min(length(x), i1+1)
      j <- max(1, j1):min(length(y), j1+1)
      # linear model of region
      ibox <- rep(i, each=length(j))
      jbox <- rep(j, length(i))
      X <- x[ibox]
      Y <- y[jbox]
      Z <- as.vector(sapply(1:length(ibox), function(k){z[ibox[k], jbox[k]]}))
      mdl <- lm( Z ~ X + Y)
      # Return interpolated altitude
      as.numeric(mdl$coef[1] + mdl$coef[2]*sn + mdl$coef[3]*ew)
    }
  }
  
  # Create the function, for use with grad()
  mw <<- makemw(x, y, z)
})