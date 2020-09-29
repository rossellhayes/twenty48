twenty48_env <- new.env()

#' @importFrom R6 R6Class

Twenty48 <- R6Class(
  "Twenty48",
  public = list(
    grid       = NULL,
    score      = 0,
    game_over  = FALSE,
    initialize = function(size = 4) {
      private$build_grid(size)
      self$play()
    },
    play = function() {
      print(self)

      while (interactive()) {
        if (self$game_over) {
          switch(
            substr(tolower(readline("> ")), 1, 1),
            r = private$restart(),
            q = {cat("\014"); break}
          )
        } else {
          switch(
            tolower(readline("> ")),
            w       = private$act("up"),
            a       = private$act("left"),
            s       = private$act("down"),
            d       = private$act("right"),
            z       = private$back(),
            restart = private$restart(),
            quit    = {cat("\014"); break}
          )
        }

        print(self)
      }
    },
    print = function() {
      cat("\014")

      if (!self$game_over) {
        cat(
          crayon::silver(
            'Move with WASD. Undo a move with "z".',
            '\nType "quit" to exit or "restart" for a new game.\n'
          )
        )
        cat(crayon::silver(paste("Score:", self$score, "\n")))
      } else {
        cat('Game over.\nType "q" to exit or "r" for a new game.\n')
        cat(paste('Score:', self$score, "\n"))
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
    previous_grid  = NULL,
    previous_score = 0,
    new_score      = list(up = 0, down = 0, left = 0, right = 0),
    moves          = list(up = 0, down = 0, left = 0, right = 0),
    previous_moves = list(up = 0, down = 0, left = 0, right = 0),
    build_grid = function(size) {
      self$game_over <- FALSE
      self$grid      <- matrix(0, nrow = size, ncol = size)
      self$grid[sample(seq_along(self$grid), 2)] <- c(2, sample(c(2, 4), 1))

      private$moves <- list(
        up    = private$move(self$grid, "up"),
        down  = private$move(self$grid, "down"),
        left  = private$move(self$grid, "left"),
        right = private$move(self$grid, "right")
      )
    },
    back = function() {
      self$grid      <- private$previous_grid
      self$score     <- private$previous_score
      private$moves  <- private$previous_moves
      self$game_over <- FALSE
      self
    },
    restart = function() {
      private$previous_grid  <- self$grid
      private$previous_score <- self$score
      private$previous_moves <- private$moves
      self$score             <- 0
      self$game_over         <- FALSE

      private$build_grid(nrow(self$grid))

      self
    },
    act = function(direction) {
      if (all(self$grid == private$moves[[direction]])) {return(self)}

      private$previous_grid  <- self$grid
      private$previous_score <- self$score
      private$previous_moves <- private$moves
      self$score             <- private$new_score[[direction]]
      self$grid              <- private$spawn(private$moves[[direction]])
      self$game_over         <- FALSE

      private$moves <- list(
        up    = private$move(self$grid, "up"),
        down  = private$move(self$grid, "down"),
        left  = private$move(self$grid, "left"),
        right = private$move(self$grid, "right")
      )

      if (
        all(self$grid != 0)                       &&
        all(self$grid == private$moves[["up"]])   &&
        all(self$grid == private$moves[["down"]]) &&
        all(self$grid == private$moves[["left"]]) &&
        all(self$grid == private$moves[["right"]])
      ) {
        self$game_over <- TRUE
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
      available <- which(grid == 0)

      if (length(available) > 1) {
        spawn_space <- sample(available, 1)
      } else {
        spawn_space <- available
      }

      grid[spawn_space] <- sample(c(2, 4), 1, prob = c(7/8, 1/8))
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
