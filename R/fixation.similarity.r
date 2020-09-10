## Tanimoto coefficient
tanimoto <- function(v1, v2) {
    A <- v1 %*% v2 # The inner product yields matrix, although only one value
    B <- sum(v1^2) + sum(v2^2) - A
    return(as.vector(A/B))
}

## Jaccard Coefficient
jaccard <- function(v1, v2) (sum(!v1&!v2)+sum(v1&v2))/length(v1)

## Gaussian kernel
gaussian.kernel <- function(x, theta = 1)
  1/(theta*sqrt(2*pi))*exp(-((x-0)^2)/(2*theta^2))

## Wrapper for calculating similarity
fix.footage.sim <- function(x1, x2,
                    similarity.fun = tanimoto, # a similarity function
                    kernel.fun = gaussian.kernel,
                    kernel.range = 4, # one-sided, including middle char
                    kernel.resolution = 1, # a char corresponds to how many points
                    kernel.theta = 1.2, # for gaussian
                    result.digits = 4)
{
  ## predefined, discretized kernel
  k <- kernel.fun(
    seq(-kernel.range, kernel.range, by = kernel.resolution),
    theta = kernel.theta)
  ## convolve fixations footage with the kernel
  x1.real <- Re(convolve(x1, rev(k), conj = TRUE, type = "open"))
  x2.real <- Re(convolve(x2, rev(k), conj = TRUE, type = "open"))
  ## calculate similarity
  similarity.fun(x1.real, x2.real)
}
