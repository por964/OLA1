library("ggplot2")

#die <- 1:6
#dice <- sample(die, size = 25000, replace = TRUE)
#dice
#tabulate(dice)

roll <- function() {
  die <- 1:6
  dice <- sample(die, size = 6, replace = TRUE)
  sum(dice)
}

rolls <- replicate(10000, roll())
qplot(rolls, binwidth = 1)

