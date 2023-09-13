################################################################################
#                                                                              #
#                   Steal like an Rtist: Creative Coding in R                  #
#                                                                              #
#                             Homage to the Square                             #
#                                 Josef Albers                                 #
#                                                                              #
#                 Ijeamaka Anyene Fumagalli and Sharla Gelfand                 #
#                              posit::conf(2023)                               #
#                              September 18, 2023                              #
#                                                                              #
################################################################################

# Packages ---------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(scales)
library(prismatic)

# Recreation -------------------------------------------------------------------

## Creating a rectangle in {ggplot2} -------------------------------------------

ggplot() +
  geom_rect(
    aes(
      xmin = 3, xmax = 7,
      ymin = 1.5, ymax = 5.5
    ),
    fill = "#F5BB1D"
  )

## Recreate --------------------------------------------------------------------

homage <- tribble(
  ~x0, ~y0, ~size,    ~color,
    0,   0,    10, "#5A9CBA",
    1, 0.5,     8, "#919EA3",
    2,   1,     6, "#F1EFDF",
    3, 1.5,     4, "#F5BB1D"
) %>%
  mutate(
    x1 = x0 + size,
    y1 = y0 + size
  )

homage

# Initial image

ggplot(homage) +
  geom_rect(aes(
    xmin = x0,
    xmax = x1,
    ymin = y0,
    ymax = y1,
    fill = color
  ))

# With scale_fill_identity()

ggplot(homage) +
  geom_rect(aes(
    xmin = x0,
    xmax = x1,
    ymin = y0,
    ymax = y1,
    fill = color
  )) +
  scale_fill_identity()

# With coord_fixed()

ggplot(homage) +
  geom_rect(aes(
    xmin = x0,
    xmax = x1,
    ymin = y0,
    ymax = y1,
    fill = color
  )) +
  scale_fill_identity() +
  coord_fixed()

# With theme_void()

ggplot(homage) +
  geom_rect(aes(
    xmin = x0,
    xmax = x1,
    ymin = y0,
    ymax = y1,
    fill = color
  )) +
  scale_fill_identity() +
  coord_fixed() +
  theme_void()

# With coord_fixed(expand = FALSE) - final image

homage_p <- ggplot(homage) +
  geom_rect(aes(
    xmin = x0,
    xmax = x1,
    ymin = y0,
    ymax = y1,
    fill = color
  )) +
  scale_fill_identity() +
  coord_fixed(expand = FALSE) +
  theme_void()

homage_p

## Save your work --------------------------------------------------------------

# With the wrong dimensions - whatever is open in your viewer pane

ggsave("homage.png", homage_p, bg = "black")

# With the correct dimensions

ggsave("homage_square.png",
  homage_p,
  width = 5,
  height = 5,
  bg = "black"
)

## Creating a small system -----------------------------------------------------

make_homage <- function(colors = c("#5A9CBA", "#919EA3", "#F1EFDF", "#F5BB1D")) {
  tribble(
    ~x0, ~y0, ~size,
      0,   0,    10,
      1, 0.5,     8,
      2,   1,     6,
      3, 1.5,     4
  ) %>%
    mutate(
      x1 = x0 + size,
      y1 = y0 + size,
      color = sample(colors, size = 4)
    ) %>%
    ggplot() +
    geom_rect(aes(
      xmin = x0, xmax = x1,
      ymin = y0, ymax = y1,
      fill = color
    )) +
    scale_fill_identity() +
    coord_fixed() +
    theme_void()
}

# Running a few times to see the different outputs

make_homage()

make_homage()

## set.seed() ------------------------------------------------------------------

# The same seed produces the same output

set.seed(1234)

make_homage()

set.seed(1234)

make_homage()

# Color ------------------------------------------------------------------------

## Hex -------------------------------------------------------------------------

palette <- c("#4d5a0b", "#f37f92", "#f49346", "#ffcd84")

show_col(palette)

## RGB -------------------------------------------------------------------------

mystery_color <- "#5A9CBA"

col2rgb(mystery_color)

## HSL -------------------------------------------------------------------------

### Hue ------------------------------------------------------------------------

mystery_color <- color(mystery_color)

# Extract hue

mystery_color %>%
  clr_extract_hue()

# Rotate hue

mystery_color %>%
  clr_rotate(degrees = 100)

### Saturation -----------------------------------------------------------------

# Extract saturation

mystery_color %>%
  clr_extract_saturation()

# Shift saturation

saturated_color <- mystery_color %>%
  clr_saturate(shift = 0.5)

desaturated_color <- mystery_color %>%
  clr_desaturate(shift = 0.5)

c(saturated_color, mystery_color, desaturated_color) %>%
  color() %>%
  plot()

# Looking at Homage palette in full saturation

homage[["color"]] %>%
  color() %>%
  clr_saturate(shift = 1) %>%
  plot()

# Homage in full saturation

tribble(
  ~x0, ~y0, ~size,
    0,   0,    10,
    1, 0.5,     8,
    2,   1,     6,
    3, 1.5,     4
) %>%
  mutate(
    x1 = x0 + size,
    y1 = y0 + size,
    color = colors_full_saturation
  ) %>%
  ggplot() +
  geom_rect(aes(
    xmin = x0, xmax = x1,
    ymin = y0, ymax = y1,
    fill = color
  )) +
  scale_fill_identity() +
  coord_fixed() +
  theme_void()

### Lightness ------------------------------------------------------------------

# Extract lightness

mystery_color %>%
  clr_extract_lightness()

# Shift lightness

lighter_color <- mystery_color %>%
  clr_lighten(shift = 0.2)

darker_color <- mystery_color %>%
  clr_darken(shift = 0.2)

c(lighter_color, mystery_color, darker_color) %>%
  color() %>%
  plot()

## Back to the Homages ---------------------------------------------------------

extract_hsl <- function(data) {
  data %>%
    mutate(
      color = color(color),
      h = clr_extract_hue(color),
      s = clr_extract_saturation(color),
      l = clr_extract_lightness(color)
    ) %>%
    select(color, h, s, l)
}

homage_hsl <- homage %>%
  extract_hsl()

homage_hsl

plot(homage_hsl[["color"]])

# Choosing color ---------------------------------------------------------------

## Monochromatic ---------------------------------------------------------------

monochromatic <- tribble(
  ~color,
  mystery_color,
  mystery_color %>% clr_lighten(shift = 0.20),
  mystery_color %>% clr_lighten(shift = 0.40),
  mystery_color %>% clr_lighten(shift = 0.60)
)

plot(monochromatic[["color"]])

make_homage(
  seed = 1,
  colors = monochromatic[["color"]]
)

## Analogous -------------------------------------------------------------------

analogous <- tribble(
  ~color,
  mystery_color %>% clr_lighten(shift = 0.20),
  mystery_color,
  mystery_color %>% clr_rotate(degrees = 30),
  mystery_color %>% clr_rotate(degrees = 60)
)

plot(analogous[["color"]])

make_homage(
  seed = 2,
  colors = analogous[["color"]]
)

# Adding contrast

analogous_better <- tribble(
  ~color,
  mystery_color %>% clr_lighten(shift = 0.50),
  mystery_color,
  mystery_color %>% clr_rotate(degrees = 30) %>%
    clr_lighten(shift = 0.25),
  mystery_color %>% clr_rotate(degrees = 60) %>%
    clr_darken(shift = 0.25)
)

plot(analogous_better[["color"]])

make_homage(
  seed = 3,
  colors = analogous_better[["color"]]
)

## Complementary ---------------------------------------------------------------

monochromatic_mini <- tribble(
  ~color,
  mystery_color %>% clr_lighten(shift = 0.20),
  mystery_color
)

complementary <- bind_rows(
  monochromatic_mini,
  monochromatic_mini %>%
    mutate(
      color = clr_rotate(color,
        degrees = 360 / 2
      )
    )
)

plot(complementary[["color"]])
make_homage(
  seed = 3,
  colors = complementary[["color"]]
)

# Changing saturation and lightness
monochromatic_mini <- tribble(
  ~color,
  mystery_color %>% clr_lighten(shift = 0.75),
  mystery_color
)

complementary <- bind_rows(
  monochromatic_mini,
  monochromatic_mini %>%
    mutate(
      color = clr_rotate(color,
        degrees = 360 / 2
      ),
      color = clr_desaturate(color,
        shift = 0.5
      )
    )
)

plot(complementary[["color"]])

make_homage(
  seed = 3,
  colors = complementary[["color"]]
)

## Triadic ---------------------------------------------------------------------

triadic <- tribble(
  ~color,
  mystery_color %>% clr_lighten(shift = 0.20),
  mystery_color,
  mystery_color %>%
    clr_rotate(degrees = 360 / 3),
  mystery_color %>%
    clr_rotate(degrees = 2 * 360 / 3)
)

plot(triadic[["color"]])

make_homage(
  seed = 5,
  colors = triadic[["color"]]
)

# Adding contrast

triadic <- tribble(
  ~color,
  mystery_color %>% clr_lighten(shift = 0.50),
  mystery_color,
  mystery_color %>%
    clr_rotate(degrees = 360 / 3) %>%
    clr_lighten(0.25),
  mystery_color %>%
    clr_rotate(degrees = 2 * 360 / 3) %>%
    clr_saturate(0.15)
)

plot(triadic[["color"]])
