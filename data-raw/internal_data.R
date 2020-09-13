library(dplyr)
library(magrittr)

scipen <- options(scipen = 1000)

x <- c(15, 9, 3)

colors <- expand.grid(r = 0:5, g = 0:5, b = 0:5) %>%
  mutate(
    across(everything(), ~ . * 3),
    fg = if_else(g < 6, "#ffffff", "#111111")
  ) %>%
  rowwise() %>%
  mutate(
    sum       = sum(c_across(c(r, g, b))),
    geom_mean = sqrt(mean(c_across(c(r, g, b)) ^ 2))
  ) %>%
  filter(sum %in% c(6 * (0:7) + 3)) %>%
  mutate(too_b = b >= r, too_g = g > r, too_r = r > 2 * g || r > 3 * b) %>%
  arrange(too_b, too_g, too_r, desc(sum), geom_mean, desc(r), desc(g)) %>%
  bind_rows(tibble(r = 15, g = 15, b = 15, sum = 45, fg = "#111111"), .)

bg <- colors[c("r", "g", "b")] %>%
  as.matrix() %>%
  as.raw() %>%
  as.character() %>%
  substr(2, 2) %>%
  paste0(., .) %>%
  matrix(ncol = 3) %>%
  apply(1, paste0, collapse = "") %>%
  paste0("#", .)

names(bg)      <- as.character(2 ^ (seq_along(bg) - 1))
names(bg)[[1]] <- "."
names(bg)[[2]] <- "4"
names(bg)[[3]] <- "2"

fg        <- colors[["fg"]]
names(fg) <- paste0(names(bg), "_fg")

options(scipen)

usethis::use_data(bg, fg, internal = TRUE, overwrite = TRUE)
