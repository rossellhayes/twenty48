Twenty48 <- R6::R6Class(
  "Twenty48",
  public = list(
    grid      = NULL,
    score     = 0,
    game_over = FALSE,
    dynamic   = FALSE,
    wait      = FALSE,

    initialize = function(size = 4, dynamic = TRUE) {
      self$set_dynamic(dynamic)
      self$wait <- isTRUE(requireNamespace("wait", quietly = TRUE))

      private$bg_styles <- bg_styles
      private$fg_styles <- fg_styles

      private$build_grid(size)
    },

    set_dynamic = function(dynamic) {
      if (dynamic && !rstudioapi::isAvailable()) {
        warning("Dynamic input is only supported in RStudio.", call. = FALSE)
        self$dynamic <- FALSE
      }

      self$dynamic <- dynamic
    },

    play = function() {
      print(self)

      while (interactive()) {
        if (self$game_over) {
          switch(
            substr(input("> "), 1, 1),
            r = private$ask_restart(),
            q = quit_game()
          )
        } else {
          switch(
            input(
              dynamic = self$dynamic,
              valid   = c("w", "a", "s", "d", "undo", "restart", "quit")
            ),
            w       = private$act("up"),
            a       = private$act("left"),
            s       = private$act("down"),
            d       = private$act("right"),
            undo    = private$back(),
            restart = private$restart(),
            quit    = quit_game()
          )
        }

        print(self)
      }
    },

    print = function() {
      clear_console()

      if (!self$game_over) {
        cat(
          crayon::silver(
            'Move with WASD. Type "undo" to go back one move.',
            '\nType "quit" to exit, or "restart" for a new game.\n'
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
      grid[]          <- format(
        grid, width = max(6, max(nchar(grid))), justify = "centre"
      )

      grid[] <- sapply(
        grid,
        function(x) {
          num <- gsub(" ", "", x)
          bg_style <- private$bg_styles[[num]]
          fg_style <- private$fg_styles[[num]]
          bg_style(fg_style(x))
        }
      )

      for (i in seq_len(nrow(self$grid))) {cat(grid[i, ], "\n", sep = "")}

      if (self$wait) {
        wait::wait_list(deliver = FALSE)
      }
    }
  ),

  private = list(
    bg_styles      = NULL,
    fg_styles      = NULL,
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

    ask_restart = function() {
      cat('Are you sure you want to restart? (y/n)\n')

      switch(
        substr(input("> "), 1, 1),
        y = private$restart(),
        n = self
      )
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
  )
)
