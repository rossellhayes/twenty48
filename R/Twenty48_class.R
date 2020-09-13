twenty48_env <- new.env()

Twenty48 <- R6::R6Class(
  "Twenty48",
  public = list(
    grid           = NULL,
    score          = 0,
    initialize = function(size = 4) {
      self$grid <- private$build_grid(size)
      self$play()
    },
    play = function() {
      print(self)

      while (interactive()) {
        switch(
          tolower(readline("> ")),
          w    = private$act("up"),
          a    = private$act("left"),
          s    = private$act("down"),
          d    = private$act("right"),
          r    = private$back(),
          quit = {cat("\014"); break}
        )

        print(self)
      }
    },
    print = function() {
      cat("\014")

      if (!private$game_over) {
        cat(
          crayon::silver(
            'Move with WASD.',
            'Undo a move with "r".',
            'Type "quit" to exit.\n'
          )
        )
        cat(crayon::silver(paste("Score:", self$score, "\n")))
      } else {
        cat('Game over. Type "quit" to exit.\n')
        cat(paste('Score:', self$score))
      }

      grid            <- self$grid
      grid[grid == 0] <- "."
      grid[]          <- paste0(" ", grid, " ")
      grid[]          <- format(grid, justify = "centre")
      grid[]          <- sapply(
        grid,
        function(x) {
          num <- gsub(" ", "", x)
          crayon::style(x, as = paste0(num, "_fg"), bg = num)
        }
      )

      for (i in seq_len(nrow(self$grid))) {cat(grid[i, ], "\n", sep = "")}
    }
  ),
  private = list(
    game_over      = FALSE,
    previous_grid  = NULL,
    previous_score = 0,
    new_score = list(up = 0, down = 0, left = 0, right = 0),
    build_grid = function(size) {
      grid <- matrix(0, nrow = size, ncol = size)
      grid[sample(seq_along(grid), 2)] <- c(2, sample(c(2, 4), 1))
      grid
    },
    back = function() {
      self$grid  <- private$previous_grid
      self$score <- private$previous_score
      self
    },
    act = function(direction) {
      moves <- list(
        up    = private$move(self$grid, "up"),
        down  = private$move(self$grid, "down"),
        left  = private$move(self$grid, "left"),
        right = private$move(self$grid, "right")
      )

      if (
        all(self$grid != 0)               &&
        all(self$grid == moves[["up"]])   &&
        all(self$grid == moves[["down"]]) &&
        all(self$grid == moves[["left"]]) &&
        all(self$grid == moves[["right"]])
      ) {
        private$game_over <- TRUE
      } else if (any(self$grid != moves[[direction]])) {
        private$previous_grid  <- self$grid
        private$previous_score <- self$score
        self$grid              <- moves[[direction]]
        self$score             <- private$new_score[[direction]]
        self$grid              <- private$spawn(self$grid)
      }

      self
    },
    move = function(grid, direction) {
      grid <- private$rotate(
        grid,
        switch(direction, up = "0", down = "180", left = "cw", right = "ccw")
      )
      grid <- private$slide(grid)
      grid <- private$combine(grid, direction)
      grid <- private$slide(grid)
      grid <- private$rotate(
        grid,
        switch(direction, up = "0", down = "180", left = "ccw", right = "cw")
      )

      grid
    },
    combine = function(grid, direction) {
      private$new_score[[direction]] <- self$score

      for (i in seq_len(nrow(grid) - 1)) {
        for (j in seq_len(ncol(grid))) {
          if (grid[[i, j]] == grid[[i + 1, j]]) {
            grid[[i, j]]     <- 2 * grid[[i, j]]
            grid[[i + 1, j]] <- 0
            private$new_score[[direction]] <-
              private$new_score[[direction]] + grid[[i, j]]
          }
        }
      }

      grid
    },
    slide = function(grid) {
      for (k in 1:nrow(grid) - 1) {
        for (i in seq_len(nrow(grid) - 1)) {
          for (j in seq_len(ncol(grid))) {
            if (grid[[i, j]] == 0) {
              grid[[i, j]]     <- grid[[i + 1, j]]
              grid[[i + 1, j]] <- 0
            }
          }
        }
      }

      grid
    },
    spawn = function(grid) {
      grid[sample(which(grid == 0), 1)] <-
        sample(c(2, 4), 1, prob = c(7/8, 1/8))
      grid
    },
    rotate = function(grid, rotation = c("0", "cw", "180", "ccw")) {
      if (rotation == "0")   {return(grid)}
      if (rotation == "cw")  {return(t(grid)[, ncol(grid):1])}
      if (rotation == "180") {return(grid[nrow(grid):1, ncol(grid):1])}
      if (rotation == "ccw") {return(t(grid)[nrow(grid):1, ])}
      stop("Invalid `rotation`")
    }
  ),
)