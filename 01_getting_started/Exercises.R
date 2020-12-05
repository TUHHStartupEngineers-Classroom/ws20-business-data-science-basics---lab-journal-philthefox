

EOQ <- function(D = 1000) {
  K <- 5
  h <- 0.25
  Q <- sqrt((2 * D * K) / h)
  Q
}

roll <- function(faces = 1:6, dice = 2) {
  probabilities_vector <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.5)
  dice <- sample(x = faces, size = dice, replace = TRUE, prob = probabilities_vector)
  sum(dice)
}

results <- replicate(n = 100, expr = roll(), simplify = TRUE)
hist(results)