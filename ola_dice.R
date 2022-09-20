roll_cheatdice <- function() {
  die <- 1:6
  cheat_dice <- sample(die, size = 2, replace = TRUE,
                       prob = c(4,1,1,1,1,4))
  sum(cheat_dice)
}

v1 <- replicate(10000, roll_cheatdice())

roll_fundice <- function() {
  die <- 1:6
  fun_dice <- sample(die, size = 2, replace = TRUE,
                     prob = c(1,0,1,0,0,1))
  sum(fun_dice)
}


v2 <- replicate(10000, roll_fundice())
t(v1)%*%v2

str(v1)

v3 <- c(2,6)
v4 <- c(-3,1)
t(v3)%*%v4

