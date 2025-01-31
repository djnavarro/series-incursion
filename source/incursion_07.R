# forked from curled

sys_id <- "07"
sys_name <- "incursion"

output_dir <- here::here("output", sys_id)
if (!dir.exists(output_dir)) dir.create(output_dir)

source(here::here("source", "core.R"), echo = FALSE)
source(here::here("source", "bezier.R"), echo = FALSE)

# base image --------------------------------------------------------------

mat_to_df <- function(mat, name) {
  as.data.frame(mat) |>
    dplyr::mutate(x = dplyr::row_number()) |>
    tidyr::pivot_longer(
      cols = -x,
      names_to = "y",
      values_to = name
    ) |>
    dplyr::mutate(y = as.integer(substr(y, 2, 100)))
}

create_base_image <- function(seed) {

  set.seed(seed)

  # set up palettes
  palettes <- c(
    "palette_01.csv",
    "palette_02.csv",
    "palette_03.csv"
  ) |>
    purrr::map(
      \(x) here::here("source", "palettes", x) |>
        readr::read_csv(show_col_types = FALSE)
    ) |>
    dplyr::bind_rows()

  n_shades <- 1024
  ind <- sample(nrow(palettes), 1)
  palette_base <- unlist(palettes[ind,])
  shades <- (colorRampPalette(palette_base))(n_shades)

  # parameters that affect all bezier objects
  pull_1 <- runif(1, min = -.1, max = .1) * .75
  pull_2 <- runif(1, min = 0, max = .2) * .75
  x_mid <- runif(1, min = -2, max = 2)
  y_mid <- runif(1, min = -2, max = 2)
  width_scale <- runif(1, min = 1, max = 2)
  max_arc <- runif(1, min = 1, max = 2) * pi/96

  get_curl <- function(x, y, seed = NULL) {
    ambient::curl_noise(
      generator = ambient::fracture,
      noise = ambient::gen_simplex,
      fractal = ambient::fbm,
      x = x,
      y = y,
      frequency = 1,
      seed = seed,
      octaves = 5
    )
  }

  n_ribbons <- 500L

  base <- tibble::tibble(
    x = runif(n_ribbons, min = -2, max = 2),
    y = runif(n_ribbons, min = -2, max = 2)
  )
  curl <- get_curl(base$x, base$y, seed) |>
    dplyr::mutate(
      x = ambient::normalize(x, to = c(-1, 1)),
      y = ambient::normalize(y, to = c(-1, 1))
    )

  values <- tibble::tibble(
    x = base$x,
    y = base$y,
    xend = x + curl$x,
    yend = y + curl$y,
    xctr_1 = (1 - pull_1) * (x + xend)/2 + pull_1 * x_mid,
    yctr_1 = (1 - pull_1) * (y + yend)/2 + pull_1 * y_mid,
    xctr_2 = (x + xend) / 2 + pull_2 * runif(n_ribbons, min = -2, max = 2),
    yctr_2 = (y + yend) / 2 + pull_2 * runif(n_ribbons, min = -2, max = 2),
    width = width_scale * runif(n_ribbons, min = .01, max = .2),
    smooth = 6L,
    n = 100L,
    fill = sample(shades, n_ribbons, replace = TRUE),
    color = fill
  )

  # list of things to draw
  drawables <- purrr::pmap(values, bezier_ribbon)

  # draw sketch and save it
  r <- 2
  tmp <- tempfile()
  png(
    filename = tmp,
    width = 200,
    height = 200,
    units = "px",
    bg = shades[1]
  )
  drawables |>
    sketch() |>
    draw(xlim = c(-r, r), ylim = c(-r, r))
  dev.off()

  mat <- png::readPNG(source = tmp)

  ht <- mat_to_df(mat[,,1], "val")
  ht$val <- ceiling(ht$val * (n_shades - 1))
  ht$size <- 1L
  ht$shade <- shades[ht$val + 1]
  ht$val <- NULL
  return(ht)
}

show_base_image <- function(ht) {
  ggplot2::ggplot(ht, ggplot2::aes(x, y, fill = shade)) +
    ggplot2::geom_raster(show.legend = FALSE) +
    ggplot2::coord_equal() +
    ggplot2::theme_void()
}

# add curl ----------------------------------------------------------------

unfold <- function(
    data,
    iterations,
    scale,
    octaves,
    noise = NULL,
    fractal = NULL,
    ...
) {

  if (is.null(noise)) noise <- ambient::gen_simplex
  if (is.null(fractal)) fractal <- ambient::billow
  seed <- data$seed[1]
  data$iteration <- 1
  data$z <- 1

  do_step <- function(data, iter) {
    n <- nrow(data)
    noise <- ambient::curl_noise(
      x = data$x,
      y = data$y,
      z = data$z,
      seed = seed,
      generator = ambient::fracture,
      noise = noise,
      fractal = fractal,
      octaves = octaves,
      ...
    )
    data$iteration <- iter
    data$x <- data$x + noise$x * scale
    data$y <- data$y + noise$y * scale
    data$z <- data$z + noise$z * scale
    return(data)
  }
  state <- purrr::accumulate(
    .x = (1:iterations) + 1,
    .f = do_step,
    .init = data
  )
  state <- dplyr::bind_rows(state)
  return(state)
}


# top-level ---------------------------------------------------------------

make_art <- function(seed) {

  cli::cli_text("building with seed ", seed)
  cli::cli_text(" - making base image")

  ht <- create_base_image(seed)

  set.seed(seed)

  cli::cli_text(" - adding curl")

  its <- 300
  dat <- ht |>
    dplyr::mutate(
      seed = seed,
      x = x * .01,
      y = y * .01
    ) |>
    unfold(
      iterations = its,
      scale = .00003,
      octaves = 7
    )

  compute_limit <- function(data, column, border = .04) {
    values <- data[[column]][data$iteration == 1]
    range <- c(1 - max(values), 1 - min(values))
    limit <- range + c(1, -1) * border
    return(limit)
  }

  cli::cli_text(" - building plot")

  pic <- ggplot2::ggplot(dat) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        x = 1 - x,
        y = 1 - y,
        color = shade,
        size = size * 10 * abs((its - iteration)/its)
      ),
      alpha = 1,
      stroke = 0,
      show.legend = FALSE
    ) +
    ggplot2::coord_cartesian(
      xlim = compute_limit(dat, "x", border = .1),
      ylim = compute_limit(dat, "y", border = .1)
    ) +
    ggplot2::scale_x_continuous(name = NULL, expand = c(0, 0), breaks = NULL) +
    ggplot2::scale_y_continuous(name = NULL, expand = c(0, 0), breaks = NULL) +
    ggplot2::scale_size_identity() +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_alpha_continuous(range = c(0, 1)) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white"))

  output <- paste0(sys_name, "_", sys_id, "_", seed, ".png")
  scaling <- 40 / 3

  output_path <- fs::path(output_dir, output)
  ggplot2::ggsave(
    filename = output_path,
    plot = pic,
    width = scaling,
    height = scaling,
    dpi = 2000 / scaling
  )
}

if (TRUE) for (s in 1700:1799) make_art(s)
