#! /usr/bin/Rscript
library(ggplot2)

MontyHall <- function(nRuns = 1000, doors = seq(3, 30)) {
  runs <- data.frame(matrix(ncol=2, nrow=0), stringsAsFactors = FALSE)
  names(runs) <- c("Doors", "Win rate")

  for (d in doors) {
    ## Random number in range
    randomDoor <- function() sample(1:d, 1)

    ## Function to return the remaining door
    remDoor <- function(set, selected) {
      prized <- set$"Door"[set$"Prize" == "CAR"]
      if (prized == selected) {
        repeat {
          r <- randomDoor()
          if (r != selected) {
            return (r)
          }
        }
      }
      return(prized)
    }

    doors <- as.data.frame(t(as.data.frame(
        lapply(seq(1, d), function(x) { (c(x, "GOAT")) } ),
        )),
        row.names = seq(1, d),
        stringsAsFactors = FALSE
        )
    names(doors) <- c("Door", "Prize")

    wins <- 0
    for (i in seq(1, nRuns)) {
      carDoor <- randomDoor()
      game <- doors
      game$"Prize"[game$"Door" == carDoor] <- "CAR"

      selectedDoor <- randomDoor()

      remainDoor <- remDoor(game, selectedDoor)

      if (game$"Prize"[game$"Door" == remainDoor] == "CAR") {
        wins <- wins + 1
      }
    }
    runs[nrow(runs) + 1,] <- c(d, wins/nRuns)
  }

  print(runs)

  ggplot(runs, aes(x=Doors, y = `Win rate`)) +
    scale_x_continuous(name = "Número de Portas") +
    scale_y_continuous(name = "Frêquencia de Vitória") +
    geom_point() +
    stat_function(fun=function(x) {1 - (1/x)}, color="blue")
}

Frequencia <- function() {
  ggplot(data.frame(x = c(3, 30)), aes(x)) +
  stat_function(fun = function(x) {1 - (1/x)}, color="blue")
}
