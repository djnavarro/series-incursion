# forked from curled

sys_id <- "25"
sys_name <- "incursion"

output_dir <- here::here("output", sys_id)
if (!dir.exists(output_dir)) dir.create(output_dir)

# base image --------------------------------------------------------------

sample_data <- function(seed = NULL, n = 100) {
  if(!is.null(seed)) set.seed(seed)
  dat <- tibble::tibble(
    x0 = runif(n),
    y0 = runif(n),
    x1 = x0 + runif(n, min = -.2, max = .2),
    y1 = y0 + runif(n, min = -.2, max = .2),
    shade = runif(n),
    size = runif(n),
    shape = factor(sample(0:22, size = n, replace = TRUE))
  )
}

styled_plot <- function(data = NULL, palette) {
  w <- runif(1)
  ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = x0,
      y = y0,
      color = w * x1 + (1 - w) * y1,
      size = size
    )
  ) +
    ggplot2::geom_point(shape = 0, show.legend = FALSE) +
    ggplot2::theme_void() +
    ggplot2::scale_colour_gradientn(colours = palette) +
    ggplot2::scale_size(range = c(2, 3))
}

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
  palette_base <- sample(palette_base, size = 8, replace = TRUE)
  shades <- (colorRampPalette(palette_base))(n_shades)

  # set up plot
  dat <- sample_data(seed, n = 1000)
  plt <- styled_plot(dat, grey.colors(4))
  tmp <- tempfile()
  ggplot2::ggsave(
    filename = tmp,
    plot = plt,
    device = "png",
    width = 160,
    height = 160,
    units = "px"
  )
  mat <- png::readPNG(source = tmp)

  ht <- mat_to_df(mat[,,1], "val")
  ht$val <- ceiling(ht$val * (n_shades - 1))
  ht$size <- 1L
  ht$shade <- shades[ht$val + 1]
  ht$val <- NULL
  ht$id <- 1:nrow(ht)
  ht$free <- runif(nrow(ht)) < .1
  return(ht)
}

show_base_image <- function(ht) {
  ggplot2::ggplot(ht, ggplot2::aes(x, y, fill = shade)) +
    ggplot2::geom_raster(show.legend = FALSE) +
    ggplot2::coord_equal() +
    ggplot2::theme_void() +
    ggplot2::scale_fill_identity()
}

# add curl ----------------------------------------------------------------

within_bounds <- function(x, y, iter = 0) {
  r0 <- 1.0
  r1 <- 0.6
  r2 <- 0.3
  outer_ring <- (x^2 + y^2 < r0^2) & (x^2 + y^2 > r1^2)
  inner_disc <- (x^2 + y^2 < r2^2)
  outer_ring | inner_disc
}

within_inner <- function(x, y) {
  r2 <- 0.3
  inner_disc <- (x^2 + y^2 < r2^2)
  inner_disc
}

within_outer <- function(x, y) {
  r0 <- 1.0
  outer_disc <- (x^2 + y^2 < r0^2)
  outer_disc
}

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
    data$x <- data$x + noise$x * scale * (data$inner - .5) * 2
    data$y <- data$y + noise$y * scale * (data$inner - .5) * 2
    data$z <- data$z + noise$z * scale * (data$inner - .5) * 2

    if (iter < 500) {
      data <- data |> dplyr::filter(within_bounds(x, y, iter) | free)
    } else if (iter == 500) {
      data <- data |>
        dplyr::filter(free) |>
        dplyr::filter(!within_bounds(x, y, iter))
    } else {
      data <- data |>
        dplyr::filter(!within_bounds(x, y, iter))
    }
    data <- data |> dplyr::filter(within_outer(x, y))

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

  its <- 2000
  dat <- ht |>
    dplyr::mutate(
      seed = seed,
      x = ambient::normalise(x, to = c(-1, 1)),
      y = ambient::normalise(y, to = c(-1, 1))
    ) |>
    dplyr::filter(within_bounds(x, y)) |>
    dplyr::mutate(inner = within_inner(x, y)) |>
    unfold(
      iterations = its,
      scale = .00002,
      octaves = 7
    ) |>
    dplyr::group_by(id) |>
    dplyr::mutate(
      size = ifelse(
        test = iteration > 500,
        yes  = 1 * (1 - (iteration - 500)/(its - 500)),
        no   = 4 * (1 - iteration/500) + 1
      )
    ) |>
    dplyr::ungroup()

  bg <- dat |>
    dplyr::count(shade) |>
    dplyr::arrange(dplyr::desc(n)) |>
    dplyr::slice_head(n = 1) |>
    dplyr::pull(shade)

  compute_limit <- function(data, column, border = .1) {
    values <- data[[column]][data$iteration == min(data$iteration)]
    limit <- range(values) + c(-1, 1) * border
    return(limit)
  }

  cli::cli_text(" - building plot")

  pic <- ggplot2::ggplot(dat) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        x = x,
        y = y,
        color = shade,
        size = size
      ),
      alpha = 1,
      stroke = 0,
      show.legend = FALSE
    ) +
    ggplot2::coord_cartesian(
      xlim = compute_limit(dat, "x", border = .4),
      ylim = compute_limit(dat, "y", border = .4)
    ) +
    ggplot2::scale_x_continuous(name = NULL, expand = c(0, 0), breaks = NULL) +
    ggplot2::scale_y_continuous(name = NULL, expand = c(0, 0), breaks = NULL) +
    ggplot2::scale_size_identity() +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_alpha_continuous(range = c(0, 1)) +
    ggplot2::theme_void() +
    ggplot2::geom_path(
      data = tibble::tibble(
        theta = seq(0, 2*pi, length.out = 100),
        x = cos(theta),
        y = sin(theta)
      ),
      mapping = ggplot2::aes(x, y),
      linewidth = 15,
      color = bg
    ) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = bg))

  output <- paste0(sys_name, "_", sys_id, "_", seed, ".png")
  scaling <- 40 / 3

  output_path <- fs::path(output_dir, output)
  ggplot2::ggsave(
    filename = output_path,
    plot = pic,
    width = scaling,
    height = scaling,
    dpi = 4000 / scaling
  )
}

if (TRUE) for (s in 3298:3399) make_art(s)
