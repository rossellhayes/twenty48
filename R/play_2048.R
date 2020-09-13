#' Play 2048 in the console
#'
#' @param size An integer giving the grid's dimensions.
#'   Defaults to `4`, which generates a 4-by-4 grid.
#'
#' @return Generates an interactive game of 2048 in the console.
#' @export

play_2048 <- function(size = 4) {
  if (!is.null(ongoing_game()) && ask_resume()) {
    return(resume_2048())
  }

  if (length(size) != 1 || !is.numeric(size) || size != size %/% 1) {
    stop("`size` must be a single integer.", call. = FALSE)
  } else if (size < 2) {
    stop("`size` must be at least 2.", call. = FALSE)
  }

  for (i in seq_len(size ^ 2 + size)) {
    do.call(
      crayon::make_style, c(as.list(bg[i]), list(bg = TRUE, colors = 256))
    )
    do.call(
      crayon::make_style, c(as.list(fg[i]), list(grey = TRUE, colors = 256))
    )
  }

  twenty48_env$twenty48_game <- Twenty48$new(size)
}

#' @rdname play_2048
#' @export

resume_2048 <- function() {
  if (is.null(ongoing_game())) {
    stop("No ongoing twenty48 game to resume.", call. = FALSE)
  }

  ongoing_game()$play()
}

ongoing_game <- function() {
  twenty48_env$twenty48_game
}

ask_resume <- function() {
  if (is.null(ongoing_game()) || ongoing_game()$game_over) {return(FALSE)}

  cat(
    "There is an ongoing game of twenty48.",
    "Do you want to resume? (y/n)"
  )

  response <- substr(readline("> "), 1, 1)

  while (TRUE) {
    switch(
      response,
      y = return(TRUE),
      n = return(FALSE),
      {response <- invalid_response()}
    )
  }
}

invalid_response <- function() {
  cat('I didn\'t understand that input. Please type "y" or "n" or press ESC.')
  substr(readline("> "), 1, 1)
}
