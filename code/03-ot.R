################################################################################
#                                                                              #
#                   Steal like an Rtist: Creative Coding in R                  #
#                                                                              #
#                               o.T. (carré noir)                              #
#                                  Vera Molnár                                 #
#                                                                              #
#                 Ijeamaka Anyene Fumagalli and Sharla Gelfand                 #
#                              posit::conf(2023)                               #
#                              September 18, 2023                              #
#                                                                              #
################################################################################

# Packages ---------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

# Creating Letter U ------------------------------------------------------------

## Coordinates of Letter U -----------------------------------------------------

u_shape <- tribble(
     ~x,   ~y,
      0,    0,
      1,    0,
      1,    1,
    2/3,    1,
    2/3,  1/3,
    1/3,  1/3,
    1/3,    1,
      0,    1
)

## Plotting Letter U -----------------------------------------------------------

u_shape %>%
  ggplot() +
  geom_polygon(
    aes(
      x = x,
      y = y
    )
  ) +
  coord_fixed(
    xlim = c(0, 2),
    ylim = c(0, 2)
  )

### Adding flexibility

create_initial_shape <- function(x0, y0) {
  tribble(
        ~x,       ~y,
    x0 + 0,   y0 + 0,
    x0 + 1,   y0 + 0,
    x0 + 1,   y0 + 1,
  x0 + 2/3,   y0 + 1,
  x0 + 2/3, y0 + 1/3,
  x0 + 1/3, y0 + 1/3,
  x0 + 1/3,   y0 + 1,
    x0 + 0,   y0 + 1,
    x0 + 0,   y0 + 0
  )
}

### with x0 = 0, y0 = 0

create_initial_shape(0, 0) %>%
  ggplot() +
  geom_polygon(
    aes(
      x = x,
      y = y
    )
  ) +
  coord_fixed(
    xlim = c(0, 2),
    ylim = c(0, 2)
  )

### with x0 = 1, y0 = 1

create_initial_shape(1, 1) %>%
  ggplot() +
  geom_polygon(
    aes(
      x = x,
      y = y
    )
  ) +
  coord_fixed(
    xlim = c(0, 2),
    ylim = c(0, 2)
  )

# Orientation of the Letter U --------------------------------------------------

## Applying trigonometry lesson ------------------------------------------------

### Step 1: Place shape

x0 <- 3
y0 <- 3

initial_u_shape <-
  create_initial_shape(x0, y0)

initial_u_shape %>%
  ggplot() +
  geom_polygon(
    aes(
      x = x,
      y = y
    )
  ) +
  geom_point(
    aes(
      x = x0,
      y = y0
    ),
    size = 5,
    color = "red"
  ) +
  coord_fixed(
    xlim = c(-1, 4),
    ylim = c(-1, 4)
  )

### Step 2: Subtract point of rotation off of each vertex

x0 <- 3
y0 <- 3

initial_u_shape <-
  create_initial_shape(x0, y0) %>%
  mutate(
    x = x - x0,
    y = y - y0
  )

initial_u_shape %>%
  ggplot() +
  geom_polygon(
    aes(
      x = x,
      y = y
    )
  ) +
  geom_point(
    aes(
      x = x0,
      y = y0
    ),
    size = 5,
    color = "red"
  ) +
  coord_fixed(
    xlim = c(-1, 4),
    ylim = c(-1, 4)
  )

### Step 3: Rotate

x0 <- 3
y0 <- 3

initial_u_shape <-
  create_initial_shape(x0, y0) %>%
  mutate(
    x = x - x0,
    y = y - y0
  ) %>%
  mutate(
    x_new = y,
    y_new = -x
  )

initial_u_shape %>%
  ggplot() +
  geom_polygon(
    aes(
      x = x_new,
      y = y_new
    )
  ) +
  geom_point(
    aes(
      x = x0,
      y = y0
    ),
    size = 5,
    color = "red"
  ) +
  coord_fixed(
    xlim = c(-1, 4),
    ylim = c(-1, 4)
  )

### Step 4: Add back point of rotation

x0 <- 3
y0 <- 3

initial_u_shape <-
  create_initial_shape(x0, y0) %>%
  mutate(
    x = x - x0,
    y = y - y0
  ) %>%
  mutate(
    x_new = y,
    y_new = -x
  ) %>%
  mutate(
    x = x_new + x0,
    y = y_new + y0
  )

initial_u_shape %>%
  ggplot() +
  geom_polygon(
    aes(
      x = x,
      y = y
    )
  ) +
  geom_point(
    aes(
      x = x0,
      y = y0
    ),
    size = 5,
    color = "red"
  ) +
  coord_fixed(
    xlim = c(-1, 4),
    ylim = c(-1, 4)
  )

### Step 5: Correct coordinates to back at "original origin"

x0 <- 3
y0 <- 3
shape_width <- 1

initial_u_shape <-
  create_initial_shape(x0, y0) %>%
  mutate(
    x = x - x0,
    y = y - y0
  ) %>%
  mutate(
    x_new = y,
    y_new = -x
  ) %>%
  mutate(
    x = x_new + x0,
    y = y_new + y0 + shape_width
  )

initial_u_shape %>%
  ggplot() +
  geom_polygon(
    aes(
      x = x,
      y = y
    )
  ) +
  geom_point(
    aes(
      x = x0,
      y = y0
    ),
    size = 5,
    color = "red"
  ) +
  coord_fixed(
    xlim = c(-1, 4),
    ylim = c(-1, 4)
  )

## Turning Lesson Into Function ------------------------------------------------

rotate_shape <- function(data, x0, y0, degrees, shape_width) {
  if (degrees == 90) {
    data %>%
      mutate(
        x = x - x0,
        y = y - y0
      ) %>%
      mutate(
        x_new = y,
        y_new = -x
      ) %>%
      mutate(
        x = x_new + x0,
        y = y_new + y0 + shape_width
      )
  } else if (degrees == 180) {
    data %>%
      mutate(
        x = x - x0,
        y = y - y0
      ) %>%
      mutate(
        x_new = -x,
        y_new = -y
      ) %>%
      mutate(
        x = x_new + x0 + shape_width,
        y = y_new + y0 + shape_width
      )
  } else if (degrees == 270) {
    data %>%
      mutate(
        x = x - x0,
        y = y - y0
      ) %>%
      mutate(
        x_new = -y,
        y_new = x
      ) %>%
      mutate(
        x = x_new + x0 + shape_width,
        y = y_new + y0
      )
  } else if (degrees == 0) {
    data
  }
}

## Turning Lesson[s] into Function[s] ------------------------------------------

create_rotate_shape <- function(x0, y0, degrees, shape_width = 1) {
  output <-
    create_initial_shape(x0 = x0, y0 = y0) %>%
    rotate_shape(
      data = .,
      x0 = x0,
      y0 = y0,
      degrees = degrees,
      shape_width = shape_width
    )

  return(output)
}

x0 <- 3
y0 <- 3

rotated_shape <-
  create_rotate_shape(
    x0 = x0,
    y0 = y0,
    degrees = 180
  )

ggplot(data = rotated_shape) +
  geom_polygon(
    aes(
      x = x,
      y = y
    )
  ) +
  coord_fixed(
    xlim = c(0, 5),
    ylim = c(0, 5)
  )

# Composition of piece ---------------------------------------------------------

## Creating the grid -----------------------------------------------------------

ncol <- 10
nrow <- 10
shape_width <- 1
perimeter_width <- shape_width + .25

grid <-
  expand_grid(
    x = seq(0,
            by = perimeter_width,
            length.out = ncol
    ),
    y = seq(0,
            by = perimeter_width,
            length.out = nrow
    )
  )

grid

### adding shift in y

grid <- expand_grid(
  x = seq(0,
    by = perimeter_width,
    length.out = ncol
  ),
  y = seq(0,
    by = perimeter_width,
    length.out = nrow
  )
) %>%
  mutate(y = if_else(x >= 5 * perimeter_width,
    y + perimeter_width / 2,
    y
  ))

grid %>%
  ggplot() +
  geom_point(
    aes(
      x = x,
      y = y
    )
  ) +
  coord_fixed()

## Creating our system ---------------------------------------------------------

make_molnar_system <- function() {
  ncol <- 10
  nrow <- 10
  shape_width <- 1
  perimeter_width <- shape_width + .25

  grid <-
    expand_grid(
      x = seq(0, by = perimeter_width, length.out = ncol),
      y = seq(0, by = perimeter_width, length.out = nrow)
    ) %>%
    mutate(y = if_else(x >= 5 * perimeter_width,
                       y + perimeter_width / 2,
                       y
    ))

  output <-
    map_dfr(
      1:nrow(grid),
      function(i) {
        create_rotate_shape(
          x = grid$x[i],
          y = grid$y[i],
          degrees = 180,
          shape_width = shape_width
        )
      }
    )

  return(output)
}

## Applying our system ---------------------------------------------------------

initial_output <-
  make_molnar_system()

initial_output %>%
  ggplot() +
  geom_polygon(aes(
    x = x,
    y = y
  )) +
  coord_fixed()

### Using group argument

make_molnar_system <- function() {
  ncol <- 10
  nrow <- 10
  shape_width <- 1
  perimeter_width <- shape_width + .25

  grid <-
    expand_grid(
      x = seq(0, by = perimeter_width, length.out = nrow),
      y = seq(0, by = perimeter_width, length.out = ncol)
    ) %>%
    mutate(y = if_else(x >= 5 * perimeter_width,
                       y + perimeter_width / 2,
                       y
    ))

  output <-
    map_dfr(
      1:nrow(grid),
      function(i) {
        bind_cols(
          group = i,
          create_rotate_shape(
            x = grid$x[i],
            y = grid$y[i],
            degrees = 180,
            shape_width = shape_width
          )
        )
      }
    )

  return(output)
}

### applying the changes

initial_output <-
  make_molnar_system()

initial_output %>%
  ggplot() +
  geom_polygon(
    aes(
      x = x,
      y = y,
      group = group
    )
  ) +
  coord_fixed()

### Incorporating rotation

make_molnar_system <- function(seed) {
  set.seed(seed)

  ncol <- 10
  nrow <- 10
  shape_width <- 1
  perimeter_width <- shape_width + .25

  grid <-
    expand_grid(
      x = seq(0, by = perimeter_width, length.out = nrow),
      y = seq(0, by = perimeter_width, length.out = ncol)
    ) %>%
    mutate(y = if_else(x >= 5 * perimeter_width,
                       y + perimeter_width / 2,
                       y
    ))

  output <-
    map_dfr(
      1:nrow(grid),
      function(i) {
        bind_cols(
          group = i,
          create_rotate_shape(
            x = grid$x[i],
            y = grid$y[i],
            degrees = sample(c(0, 90, 180, 270),
                             size = 1
            ),
            shape_width = shape_width
          )
        )
      }
    )

  return(output)
}

### plotting changes

initial_output <-
  make_molnar_system(seed = 150)

initial_output %>%
  ggplot() +
  geom_polygon(
    aes(
      x = x,
      y = y,
      group = group
    )
  ) +
  coord_fixed()

# Patterns vs. Pure Random -----------------------------------------------------

## Example

set.seed(150)
degree_pattern_options <- c(0, 90, 180, 270)
degree_pattern <- sample(degree_pattern_options, size = 6, replace = TRUE)
degree_pattern

## Incorporating into system

make_molnar_system <-
  function(seed) {
    set.seed(seed)

    ncol <- 10
    nrow <- 10
    shape_width <- 1
    perimeter_width <- shape_width + .25

    degree_pattern_options <- c(0, 90, 180, 270)
    degree_pattern <- sample(degree_pattern_options, size = 6, replace = TRUE)

    grid <-
      expand_grid(
        x = seq(0,
                by = perimeter_width,
                length.out = nrow
        ),
        y = seq(0,
                by = perimeter_width,
                length.out = ncol
        )
      ) %>%
      mutate(y = if_else(x >= 5 * perimeter_width,
                         y + perimeter_width / 2,
                         y
      ))

    degree_pattern_exp <- rep(degree_pattern,
                              length.out = nrow(grid)
    )

    output <-
      map_dfr(
        1:nrow(grid),
        function(i) {
          bind_cols(
            group = i,
            create_rotate_shape(
              x = grid$x[i],
              y = grid$y[i],
              degrees = degree_pattern_exp[i],
              shape_width = shape_width
            )
          )
        }
      )

    return(output)
  }

## Plotting output, seed = 150

final_output <-
  make_molnar_system(seed = 150)

final_output %>%
  ggplot() +
  geom_polygon(
    aes(
      x = x,
      y = y,
      group = group
    )
  ) +
  coord_fixed()

## Plotting output, seed = 55

final_output <-
  make_molnar_system(seed = 55)

final_output %>%
  ggplot() +
  geom_polygon(
    aes(
      x = x,
      y = y,
      group = group
    )
  ) +
  coord_fixed()


# Final Touches ----------------------------------------------------------------

final_output <-
  make_molnar_system(seed = 150)

## updating fill

final_output %>%
  ggplot() +
  geom_polygon(
    aes(
      x = x,
      y = y,
      group = group
    ),
    fill = "#1a1617",
    color = "#1a1617"
  ) +
  coord_fixed()

## adding theme_void()

final_output %>%
  ggplot() +
  geom_polygon(
    aes(
      x = x,
      y = y,
      group = group
    ),
    fill = "#1a1617",
    color = "#1a1617"
  ) +
  coord_fixed() +
  theme_void()

## updating plot.background

final_output %>%
  ggplot() +
  geom_polygon(
    aes(
      x = x,
      y = y,
      group = group
    ),
    fill = "#1a1617",
    color = "#1a1617"
  ) +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background =
      element_rect(
        fill = "#e8e6e5",
        color = "#e8e6e5"
      )
  )