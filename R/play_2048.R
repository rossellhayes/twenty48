game_env <- new.env()

ongoing_game <- function() {
  game_env[["2048"]]
}

#' Play 2048 in the console
#'
#' Interact with the game by typing commands into the console and
#' pressing enter.
#'
#' @param size An integer giving the grid's dimensions.
#'   Defaults to `4`, which generates a 4-by-4 grid.
#' @param dynamic If `dynamic` is `TRUE`, commands can be input without pressing
#'   enter, creating a more seamless game experience.
#'   Note that `dynamic` commands currently only work in RStudio.
#'
#' @return Generates an interactive game of 2048 in the console.
#' @aliases 2048 twenty48
#' @export
#' @importFrom R6 R6Class
#'
#' @examples
#' play_2048()
#' play_2048(size = 5)

play_2048 <- function(size = 4, dynamic = rstudioapi::isAvailable()) {
  if (!interactive()) {return(invisible(NULL))}

  if (rstudioapi::isAvailable()) {
    old <- rstudioapi::readRStudioPreference("console_code_completion", TRUE)
    on.exit(rstudioapi::writeRStudioPreference("console_code_completion", old))
    rstudioapi::writeRStudioPreference("console_code_completion", FALSE)
  }

  if (!is.null(ongoing_game) && ask_resume()) {
    ongoing_game()$set_dynamic(dynamic)
    return(resume_2048())
  }

  if (length(size) != 1 || !is.numeric(size) || size != size %/% 1) {
    stop("`size` must be a single integer.", call. = FALSE)
  } else if (size < 2) {
    stop("`size` must be at least 2.", call. = FALSE)
  }

  for (i in seq_along(bg)) {
    do.call(
      crayon::make_style, c(as.list(bg[i]), list(bg = TRUE, colors = 256))
    )
    do.call(
      crayon::make_style, c(as.list(fg[i]), list(grey = TRUE, colors = 256))
    )
  }

  game_env[["2048"]] <- Twenty48$new(size, dynamic)
  game_env[["2048"]]$play()
}

#' @rdname play_2048
#' @export

resume_2048 <- function() {
  if (is.null(ongoing_game())) {
    stop("No ongoing twenty48 game to resume.", call. = FALSE)
  }

  ongoing_game()$play()
}

ask_resume <- function() {
  if (is.null(ongoing_game()) || ongoing_game()$game_over) {return(FALSE)}

  cat(
    "There is an ongoing game of twenty48.",
    "Do you want to resume? (y/n)"
  )

  response <- substr(input("> "), 1, 1)

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
  substr(input("> "), 1, 1)
}

quit_game <- function() {
  do.call("return", list(clear_console()), envir = sys.frame(-1))
}

input <- function(prompt = "> ", dynamic = FALSE, valid = NULL) {
  if (dynamic) {
    while (TRUE) {
      input <- tolower(rstudioapi::getConsoleEditorContext()$contents)
      if (input %in% valid || is.null(valid) && input != "") {
        rstudioapi::sendToConsole("", execute = FALSE)
        return(input)
      }
      Sys.sleep(1/60)
    }
  }

  tolower(readline(prompt = prompt))
}

clear_console <- function() {
  if (rstudioapi::isAvailable()) {
    # Within the RStudio console, "\f" clears output
    return(cat("\f"))
  }

  # In the terminal, "\033c\033[3J" clears output
  cat("\033c\033[3J")
}
