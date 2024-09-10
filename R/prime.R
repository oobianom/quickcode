

#' Function to calculate prime numbers up to n
#' Example usage: Calculate all primes up to 100
#' @param n number of primes
#' primes(100)
#' @export

primes <- function(n) {
  if (n < 2) {
    return(integer(0)) # No primes less than 2
  }

  # Initialize a logical vector to keep track of prime status
  is_prime <- rep(TRUE, n)
  is_prime[1] <- FALSE # 1 is not a prime number

  # Implement the Sieve of Eratosthenes
  for (i in 2:sqrt(n)) {
    if (is_prime[i]) {
      is_prime[seq(i^2, n, i)] <- FALSE
    }
  }

  # Return the indices of the TRUE values, which are prime numbers
  return(which(is_prime))
}

