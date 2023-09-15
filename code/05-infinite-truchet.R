################################################################################
#                                                                              #
#                   Steal like an Rtist: Creative Coding in R                  #
#                                                                              #
#                               Untitled sketch                                #
#                                 Roni Kaufman                                 #
#                                                                              #
#                 Ijeamaka Anyene Fumagalli and Sharla Gelfand                 #
#                              posit::conf(2023)                               #
#                              September 18, 2023                              #
#                                                                              #
################################################################################

# Packages ---------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(ggforce)

# Who actually needs trigonometric functions -----------------------------------

## Corner 1 --------------------------------------------------------------------

x <- 0
y <- 0
sq_width <- 15

tribble(
  ~x0, ~y0, ~start,     ~end,
    x,   y,      0,   pi / 2
)


## Corner 2 --------------------------------------------------------------------

x <- 0
y <- 0
sq_width <- 15

tribble(
           ~x0, ~y0,       ~start,     ~end,
             x,   y,          0,     pi / 2,
  x + sq_width,   y, 3 * pi / 2,     2 * pi
)

## Corner 3 --------------------------------------------------------------------

x <- 0
y <- 0
sq_width <- 15


tribble(
           ~x0,          ~y0,       ~start,         ~end,
             x,            y,            0,       pi / 2,
  x + sq_width,            y,   3 * pi / 2,       2 * pi,
  x + sq_width, y + sq_width,           pi,    3 * pi / 2
  )


## Corner 4 --------------------------------------------------------------------

x <- 0
y <- 0
sq_width <- 15

tribble(
             ~x0,            ~y0,       ~start,         ~end,
               x,              y,            0,       pi / 2,
    x + sq_width,              y,   3 * pi / 2,       2 * pi,
    x + sq_width,   y + sq_width,           pi,   3 * pi / 2,
               x,   y + sq_width,       pi / 2,           pi
  )

## Wrap it up in a function ----------------------------------------------------

set_params <- function(x, y, sq_width) {
  tribble(
             ~x0,            ~y0,       ~start,         ~end,
               x,              y,            0,       pi / 2,
    x + sq_width,              y,   3 * pi / 2,       2 * pi,
    x + sq_width,   y + sq_width,           pi,   3 * pi / 2,
               x,   y + sq_width,       pi / 2,           pi
  )
}

# Building a system ------------------------------------------------------------

## Number of arcs --------------------------------------------------------------

### tile_type == 1

set_params <-
  function(x, y, sq_width, tile_type) {
    if (tile_type == 1) {
      tribble(
             ~x0,            ~y0,       ~start,         ~end,
               x,              y,            0,       pi / 2,
    x + sq_width,              y,   3 * pi / 2,       2 * pi,
    x + sq_width,   y + sq_width,           pi,   3 * pi / 2,
               x,   y + sq_width,       pi / 2,           pi
    ) %>%
        mutate(num_arcs = c(3, 4, 3, 4))
    }
  }

## tile_type == 2

set_params <-
  function(x, y, sq_width, tile_type) {
    if (tile_type == 1) {
      tribble(
             ~x0,            ~y0,       ~start,         ~end,
               x,              y,            0,       pi / 2,
    x + sq_width,              y,   3 * pi / 2,       2 * pi,
    x + sq_width,   y + sq_width,           pi,   3 * pi / 2,
               x,   y + sq_width,       pi / 2,           pi
    ) %>%
        mutate(num_arcs = c(3, 4, 3, 4))
    } else if (tile_type == 2) {
      tribble(
             ~x0,            ~y0,       ~start,         ~end,
               x,              y,            0,       pi / 2,
    x + sq_width,              y,   3 * pi / 2,       2 * pi,
    x + sq_width,   y + sq_width,           pi,   3 * pi / 2,
               x,   y + sq_width,       pi / 2,           pi
    ) %>%
        mutate(num_arcs = c(5, 2, 5, 2))
    }
  }

## set_params_output()

set_params(
  x = 0,
  y = 0,
  sq_width = 15,
  tile_type = 1
)

## Creating the grid -----------------------------------------------------------

sq_width <- 15
ncol <- 4
nrow <- 4

grid <-
  expand_grid(
    y = seq(
      from = 0,
      by = sq_width,
      length.out = nrow
    ),
    x = seq(
      from = 0,
      by = sq_width,
      length.out = ncol
    )
  )

## Tile selection --------------------------------------------------------------

### Recreating Infinite Truchet

sq_width <- 15
ncol <- 4
nrow <- 4

grid <-
  expand_grid(
    y = seq(
      from = 0,
      by = sq_width,
      length.out = nrow
    ),
    x = seq(
      from = 0,
      by = sq_width,
      length.out = ncol
    )
  ) %>%
  mutate(
    tile_type =
      c(
        1, 1, 2, 2,
        1, 1, 1, 1,
        1, 2, 1, 1,
        1, 1, 1, 1
      )
  )

grid

### Incorporating the element of chance

sq_width <- 15
ncol <- 4
nrow <- 4

grid <-
  expand_grid(
    y = seq(
      from = 0,
      by = sq_width,
      length.out = nrow
    ),
    x = seq(
      from = 0,
      by = sq_width,
      length.out = ncol
    )
  ) %>%
  mutate(
    tile_type =
      sample(c(1, 2),
        size = n(),
        replace = TRUE
      )
  )

grid


## Creating our system ---------------------------------------------------------

truchet_tiles <-
  function(seed) {
    ncol <- 4
    nrow <- 4
    sq_width <- 15

    set.seed(seed)

    grid <-
      expand_grid(
        y = seq(from = 0, by = sq_width, length.out = ncol),
        x = seq(from = 0, by = sq_width, length.out = nrow)
      ) %>%
      mutate(tile_type = sample(c(1, 2), size = n(), replace = TRUE))

    params_grid <-
      map_dfr(
        1:nrow(grid),
        function(i) {
          set_params(
            x = grid$x[i],
            y = grid$y[i],
            sq_width = sq_width,
            tile_type = grid$tile_type[i]
          )
        }
      )

    return(params_grid)
  }

### applying our system

output_grid <-
  truchet_tiles(seed = 150)

output_grid

output_grid %>%
  ggplot() +
  geom_arc_bar(aes(
    x0 = x0,
    y0 = y0,
    start = start,
    end = end,
    r0 = num_arcs,
    r = num_arcs + 1
  )) +
  coord_fixed()

## Converting num_arcs to radius -----------------------------------------------

# initial version
r0 <- seq(from = 1, to = 7, by = 2)
r0
r <- seq(from = 2, to = 8, by = 2)
r

## adding flexibility
num_arcs <- 4
r0 <- seq(from = 1, by = 2, length.out = num_arcs)
r0
r <- seq(from = 2, by = 2, length.out = num_arcs)
r

## Updating our system ---------------------------------------------------------

truchet_tiles <-
  function(seed) {
    ncol <- 4
    nrow <- 4
    sq_width <- 15

    set.seed(seed)

    grid <-
      expand_grid(
        y = seq(from = 0, by = sq_width, length.out = ncol),
        x = seq(from = 0, by = sq_width, length.out = nrow)
      ) %>%
      mutate(tile_type = sample(c(1, 2), size = n(), replace = TRUE))

    params_grid <-
      map_dfr(
        1:nrow(grid),
        function(i) {
          set_params(
            x = grid$x[i],
            y = grid$y[i],
            sq_width = sq_width,
            tile_type = grid$tile_type[i]
          )
        }
      )

    output <-
      map_dfr(
        1:nrow(params_grid),
        function(i) {
          bind_cols(
            slice(params_grid, i),
            r0 = seq(from = 1, by = 2, length.out = params_grid$num_arcs[i]),
            r = seq(from = 2, by = 2, length.out = params_grid$num_arcs[i])
          )
        }
      )


    return(output)
  }

### Applying our system

output_grid <-
  truchet_tiles(seed = 150)

output_grid %>%
  ggplot() +
  geom_arc_bar(aes(
    x0 = x0,
    y0 = y0,
    start = start,
    end = end,
    r0 = r0,
    r = r
  )) +
  coord_fixed()

# Color, Fill ------------------------------------------------------------------

## Color order is the same in every corner

color <- c("#fffbe6", "#fc8405")

max_num_arcs <- 5
color_seq <- rep(color,
  length.out = max_num_arcs
)
color_seq

## Updating our system

truchet_tiles <-
  function(color1,
           color2,
           seed) {
    ncol <- 4
    nrow <- 4
    sq_width <- 15
    max_num_arcs <- 5

    set.seed(seed)

    grid <-
      expand_grid(
        y = seq(from = 0, by = sq_width, length.out = ncol),
        x = seq(from = 0, by = sq_width, length.out = nrow)
      ) %>%
      mutate(tile_type = sample(c(1, 2), size = n(), replace = TRUE))

    params_grid <-
      map_dfr(
        1:nrow(grid),
        function(i) {
          set_params(
            x = grid$x[i],
            y = grid$y[i],
            sq_width = sq_width,
            tile_type = grid$tile_type[i]
          )
        }
      )

    color <- c(color1, color2)
    color_seq <- rep(color, length.out = max_num_arcs)

    output <-
      map_dfr(
        1:nrow(params_grid),
        function(i) {
          bind_cols(
            slice(params_grid, i),
            r0 = seq(from = 1, by = 2, length.out = params_grid$num_arcs[i]),
            r = seq(from = 2, by = 2, length.out = params_grid$num_arcs[i]),
            color = color_seq[1:params_grid$num_arcs[i]]
          )
        }
      )


    return(output)
  }

## Applying our system

output_grid <-
  truchet_tiles(
    seed = 150,
    color1 = "#fffbe6",
    color2 = "#fc8405"
  )

output_grid


# Final Touches ----------------------------------------------------------------

output_grid %>%
  ggplot() +
  geom_arc_bar(aes(
    x0 = x0,
    y0 = y0,
    start = start,
    end = end,
    r0 = r0,
    r = r,
    fill = color
  )) +
  coord_fixed() +
  scale_fill_identity()

## `color=` to NA

output_grid %>%
  ggplot() +
  geom_arc_bar(
    aes(
      x0 = x0,
      y0 = y0,
      start = start,
      end = end,
      r0 = r0,
      r = r,
      fill = color
    ),
    color = NA
  ) +
  coord_fixed() +
  scale_fill_identity()

## `color=` to NA, black background

output_grid %>%
  ggplot() +
  geom_arc_bar(
    aes(
      x0 = x0,
      y0 = y0,
      start = start,
      end = end,
      r0 = r0,
      r = r,
      fill = color
    ),
    color = NA
  ) +
  coord_fixed() +
  scale_fill_identity() +
  theme_void() +
  theme(panel.background = element_rect(
    fill = "black",
    color = "black"
  ))

## color = color

output_grid %>%
  ggplot() +
  geom_arc_bar(aes(
    x0 = x0,
    y0 = y0,
    start = start,
    end = end,
    r0 = r0,
    r = r,
    fill = color,
    color = color
  )) +
  coord_fixed() +
  scale_fill_identity() +
  scale_color_identity()

## `expand=` set to FALSE

output_grid %>%
  ggplot() +
  geom_arc_bar(aes(
    x0 = x0,
    y0 = y0,
    start = start,
    end = end,
    r0 = r0,
    r = r,
    fill = color,
    color = color
  )) +
  coord_fixed(expand = FALSE) +
  scale_fill_identity() +
  scale_color_identity()

## Final Piece

output_grid %>%
  ggplot() +
  geom_arc_bar(aes(
    x0 = x0,
    y0 = y0,
    start = start,
    end = end,
    r0 = r0,
    r = r,
    fill = color,
    color = color
  )) +
  coord_fixed() +
  scale_fill_identity() +
  scale_color_identity() +
  theme_void() +
  theme(
    panel.background =
      element_rect(
        fill = "#050505",
        color = "#050505"
      )
  )
