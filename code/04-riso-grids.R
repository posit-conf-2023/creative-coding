################################################################################
#                                                                              #
#                   Steal like an Rtist: Creative Coding in R                  #
#                                                                              #
#                                  Riso Grids                                  #
#                                Ryan Miglinczy                                #
#                                                                              #
#                 Ijeamaka Anyene Fumagalli and Sharla Gelfand                 #
#                              posit::conf(2023)                               #
#                              September 18, 2023                              #
#                                                                              #
################################################################################

# Packages ---------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggforce)
library(purrr)
library(magick)

# Creating polygon shapes ------------------------------------------------------

## Oreo square -----------------------------------------------------------------

# Outer portion

oreo_square_outer <- tribble(
   ~x,  ~y,
  1.5, 1.5,
  8.5, 1.5,
  8.5, 8.5,
  1.5, 8.5
) %>%
  mutate(subgroup = 1)

oreo_square_hole <- tribble(
    ~x,   ~y,
  2.75, 2.75,
  7.25, 2.75,
  7.25, 7.25,
  2.75, 7.25
) %>%
  mutate(subgroup = 2)

oreo_square_outer <- bind_rows(
  oreo_square_outer,
  oreo_square_hole
)

oreo_square_outer

ggplot() +
  geom_polygon(
    data = oreo_square_outer,
    aes(
      x = x, y = y,
      subgroup = subgroup
    )
  ) +
  coord_fixed()

# Whole shape

oreo_square_inner <- tribble(
  ~x, ~y,
   4,  4,
   6,  4,
   6,  6,
   4,  6
)

oreo_square <- bind_rows(
  oreo_square_outer %>% mutate(group = 1),
  oreo_square_inner %>% mutate(group = 2)
)

oreo_square

grey <- "#8F8E93"

ggplot() +
  geom_polygon(
    data = oreo_square,
    aes(
      x = x, y = y,
      group = group,
      subgroup = subgroup
    ),
    fill = grey
  ) +
  coord_fixed(xlim = c(0, 10), ylim = c(0, 10)) +
  theme_void()

## Upper triangle --------------------------------------------------------------

upper_triangle <- tribble(
  ~x, ~y,
   0,  0,
  10, 10,
   0, 10
) %>%
  mutate(
    group = 1,
    subgroup = 1
  )

upper_triangle

yellow <- "#DDA710"

ggplot() +
  geom_polygon(
    data = upper_triangle,
    aes(
      x = x, y = y,
      group = group,
      subgroup = subgroup
    ),
    fill = yellow
  ) +
  coord_fixed() +
  theme_void()

## Lower triangle --------------------------------------------------------------

lower_triangle <- tribble(
  ~x, ~y,
   0,  0,
  10,  0,
  10, 10
) %>%
  mutate(
    group = 1,
    subgroup = 1
  )

lower_triangle

ggplot() +
  geom_polygon(
    data = lower_triangle,
    aes(
      x = x, y = y,
      group = group,
      subgroup = subgroup
    ),
    fill = yellow
  ) +
  coord_fixed() +
  theme_void()

# Creating circle shapes

## Circles ---------------------------------------------------------------------

ggplot() +
  geom_circle(
    aes(
      x0 = 0,
      y0 = 0,
      r = 10
    )
  ) +
  coord_fixed()

## Parts of circles ------------------------------------------------------------

ggplot() +
  geom_arc_bar(
    aes(
      x0 = 0,
      y0 = 0,
      r = 10,
      r0 = 0,
      start = 0,
      end = pi / 2
    )
  ) +
  coord_fixed()

## Parts of donuts -------------------------------------------------------------

ggplot() +
  geom_arc_bar(
    aes(
      x0 = 0,
      y0 = 0,
      r = 10,
      r0 = 5,
      start = 0,
      end = pi / 2
    )
  ) +
  coord_fixed()

## Quarter donuts --------------------------------------------------------------

### Quadrant 1 -----------------------------------------------------------------

quarter_donut_1 <- tibble(
  x = 0,
  y = 0,
  r = 10,
  r0 = 5,
  start = 0,
  end = pi / 2
)

ggplot() +
  geom_arc_bar(
    data = quarter_donut_1,
    aes(
      x0 = x, y0 = y,
      r = r, r0 = r0,
      start = start, end = end
    ),
    fill = yellow,
    color = NA
  ) +
  coord_fixed() +
  theme_void()

### Quadrant 2 -----------------------------------------------------------------

quarter_donut_2 <- tibble(
  x = 0,
  y = 10,
  r = 10,
  r0 = 5,
  start = pi / 2,
  end = pi
)
ggplot() +
  geom_arc_bar(
    data = quarter_donut_2,
    aes(
      x0 = x, y0 = y,
      r = r, r0 = r0,
      start = start, end = end
    ),
    fill = grey,
    color = NA
  ) +
  coord_fixed() +
  theme_void()

quarter_donut_2

### Quadrant 3 -----------------------------------------------------------------

quarter_donut_3 <- tibble(
  x = 10,
  y = 10,
  r = 10,
  r0 = 5,
  start = pi,
  end = 3 * pi / 2
)

### Quadrant 4 -----------------------------------------------------------------

quarter_donut_4 <- tibble(
  x = 10,
  y = 0,
  r = 10,
  r0 = 5,
  start = 3 * pi / 2,
  end = 2 * pi
)

quarter_donut_4

# Putting it all together ------------------------------------------------------

## Generate shape data ---------------------------------------------------------

generate_shape_data <- function(shape, color) {
  switch(shape,
    "oreo square" = oreo_square,
    "upper triangle" = upper_triangle,
    "lower triangle" = lower_triangle,
    "quarter donut q1" = quarter_donut_1,
    "quarter donut q2" = quarter_donut_2,
    "quarter donut q3" = quarter_donut_3,
    "quarter donut q4" = quarter_donut_4
  ) %>%
    mutate(
      color = color,
      shape = shape
    )
}

yellow_oreo_square <- generate_shape_data(
  "oreo square",
  yellow
)

yellow_oreo_square

## Shape plotter ---------------------------------------------------------------

generate_shape_plotter <- function(data) {
  shape <- data[["shape"]][[1]]

  if (shape %in% c("oreo square", "upper triangle", "lower triangle")) {
    geom_polygon(
      data = data,
      aes(
        x = x, y = y, group = group, subgroup = subgroup, fill = color
      )
    )
  } else if (shape %in% c("quarter donut q1", "quarter donut q2", "quarter donut q3", "quarter donut q4")) {
    geom_arc_bar(
      data = data,
      aes(
        x0 = x, y0 = y, r = r, r0 = r0, start = start, end = end, fill = color
      ),
      color = NA
    )
  }
}

generate_shape_plotter(yellow_oreo_square)

ggplot() +
  generate_shape_plotter(yellow_oreo_square) +
  scale_fill_identity() +
  coord_fixed(xlim = c(0, 10), ylim = c(0, 10)) +
  theme_void()

## Grid of shapes --------------------------------------------------------------

layer_1 <- tribble(
  ~row, ~column,             ~shape,
     1,       1, "quarter donut q3",
     1,       2,   "upper triangle",
     2,       2, "quarter donut q1",
     2,       1,   "lower triangle"
)  %>%
  mutate(
    color = yellow,
    id = row_number()
  )

layer_1

layer_1 %>%
  split(.$id) %>%
  map(function(data) {
    generate_shape_data(data[["shape"]], data[["color"]])
  })

layer_1_plotting <- layer_1 %>%
  split(.$id) %>%
  map(function(data) {
    generate_shape_data(data[["shape"]], data[["color"]])
  }) %>%
  map(function(data) {
    data %>%
      generate_shape_plotter()
  })

layer_1_plotting

ggplot() +
  layer_1_plotting +
  scale_fill_identity() +
  coord_fixed() +
  theme_void()

## Shifting data ---------------------------------------------------------------

layer_1_data <- layer_1 %>%
  split(.$id) %>%
  map_dfr(
    function(data) {
      generate_shape_data(data[["shape"]], data[["color"]])
    },
    .id = "id"
  ) %>%
  mutate(id = as.numeric(id)) %>%
  left_join(layer_1, by = c("id", "color", "shape"))

layer_1_data

grid_size <- 10

layer_1_data_shifted <- layer_1_data %>%
  mutate(
    x = x + column * grid_size,
    y = y + row * grid_size
  )

layer_1_plotting <- layer_1_data_shifted %>%
  split(.$id) %>%
  map(function(data) {
    data %>%
      generate_shape_plotter()
  })

ggplot() +
  layer_1_plotting +
  scale_fill_identity() +
  coord_fixed() +
  theme_void()

## Layer 2 ---------------------------------------------------------------------

layer_2 <- tribble(
  ~row, ~column,             ~shape,
     1,       1,      "oreo square",
     1,       2, "quarter donut q2",
     2,       2,      "oreo square",
     2,       1, "quarter donut q4"
) %>%
  mutate(
    color = grey,
    id = row_number()
  )

layer_2_plotting <- layer_2 %>%
  split(.$id) %>%
  map_dfr(
    function(data) {
      generate_shape_data(data[["shape"]], data[["color"]])
    },
    .id = "id"
  ) %>%
  mutate(id = as.numeric(id)) %>%
  left_join(layer_2, by = c("id", "color", "shape")) %>%
  mutate(
    x = x + column * grid_size,
    y = y + row * grid_size
  ) %>%
  split(.$id) %>%
  map(function(data) {
    generate_shape_plotter(data)
  })

ggplot() +
  layer_2_plotting +
  scale_fill_identity() +
  coord_fixed() +
  theme_void()

## Putting them together -------------------------------------------------------

ggplot() +
  list(
    layer_1_plotting,
    layer_2_plotting
  ) +
  scale_fill_identity() +
  coord_fixed() +
  theme_void()

# Risograph effect -------------------------------------------------------------

## Saving plots as images ------------------------------------------------------

layer_1 <- ggplot() +
  layer_1_plotting +
  scale_fill_identity() +
  coord_fixed() +
  theme_void()

layer_1_file <- "riso-layer-1.png"

ggsave(layer_1_file, layer_1, height = 5, width = 5)

layer_2 <- ggplot() +
  layer_2_plotting +
  scale_fill_identity() +
  coord_fixed() +
  theme_void()

layer_2_file <- "riso-layer-2.png"

ggsave(layer_2_file, layer_2, height = 5, width = 5)
layer_1_data_shifted

## Risograph effect using multiply ---------------------------------------------

layer_1_img <- image_read(layer_1_file)
layer_2_img <- image_read(layer_2_file)

paper <- image_blank(
  width = image_info(layer_1_img)[["width"]],
  height = image_info(layer_1_img)[["height"]],
  color = "#E1DED7"
)

paper %>%
  image_composite(layer_1_img) %>%
  image_composite(layer_2_img,
    operator = "multiply"
  )
