## convert R2WinBUGS output to coda/mcmc
as.mcmc.bugs <- function(x) {
  if (!require("coda")) stop("coda is required to use as.mcmc.bugs")
  if (x$n.chains>1) {
    z <- list()
    for (i in 1:x$n.chains) {
      z[[i]] <- mcmc(x$sims.array[,i,],start=1,thin=x$n.thin)
    }
    class(z) <- "mcmc.list"
  } else {
    z <- mcmc(x$sims.matrix,start=1,thin=x$n.thin)
  } 
  return(z)
}
