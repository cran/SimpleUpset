## ----set-opts, include = FALSE------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, message = FALSE, warning = FALSE,
  fig.width = 10, fig.height = 8,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(tidyverse)
library(SimpleUpset)
library(pander)
theme_set(theme_bw())

## ----load-movies--------------------------------------------------------------
movies <- system.file("extdata", "movies.tsv.gz", package = "SimpleUpset") %>%
  read_tsv() %>%
  mutate(
    Decade = fct_inorder(Decade) %>% fct_rev()
  )

## ----tbl-movies, echo = FALSE-------------------------------------------------
movies %>% 
  count(Decade) %>% 
  pander(
    caption = "Summary of movies by decade",
    justify = "lr"
  )

## ----simple-upset-------------------------------------------------------------
sets <- c("Action", "Comedy", "Drama", "Thriller", "Romance")
simpleUpSet(movies, sets)

## ----default-sets-------------------------------------------------------------
default_set_layers(dry_run = TRUE)

## ----upset-decade-------------------------------------------------------------
simpleUpSet(
  movies, sets, min_size = 20,
  intersect_layers = default_intersect_layers(
    fill = "Decade",
    scale_fill_brewer(palette = "Paired"),
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.99, 0.99),
      legend.justification.inside = c(1, 1)
    )
  ),
  set_layers = default_set_layers(
    fill = "Decade", scale_fill_brewer(palette = "Paired"),
    guides(fill = guide_none()), expand = c(0.3, 0)
  )
)

## ----upset-highlights---------------------------------------------------------
## Define the sets to be coloured by name, the use scale_fill_manual
set_cols <- c(
  Action = "red", Comedy = "grey23", Drama = "red", 
  Romance = "grey23", Thriller = "grey23"
)
set_list <- default_set_layers(fill = "set", scale_fill_manual(values = set_cols))
## Use the highlights to colour the intersection bars based on the column 
## 'highlight', in conjunction with the case_when statement
intersect_list <- default_intersect_layers(
    fill = "highlight", scale_fill_manual(values = "red", na.value = "grey23")
  )
## When passing the 'highlight' column, this will be passed to both points
## and segments. Each layer can be manually edited to override this if preferred
grid_list <- default_grid_layers(
  colour = "highlight", scale_colour_manual(values = "red", na.value = "grey23")
)
simpleUpSet(
  movies, sets, min_size = 20,
  set_layers = set_list,
  intersect_layers = intersect_list,
  grid_layers = grid_list,
  sort_intersect = list(highlight, desc(size)),
  highlight = case_when(Action & Drama ~ TRUE)
) &
  plot_annotation(title = "Using Highlights") &
  theme(legend.position = "none", plot.title = element_text(hjust = 2/3))

## ----upset-highlights2--------------------------------------------------------
simpleUpSet(
  movies, sets, min_size = 20, 
  intersect_layers = default_intersect_layers(
    fill ="Comedy", 
    scale_fill_manual(values = c("grey23", "blue")), 
    guides(fill = guide_none())
  ),
  grid_layers = default_grid_layers(
    colour = "Comedy", scale_colour_manual(values = c("grey23", "blue"))
  )
)

## ----upset-boxplot, fig.height=8----------------------------------------------
## Add a simple boxplot
simpleUpSet(
  movies, sets, n_intersect = 10,
  set_layers = default_set_layers(expand = 0.3),
  intersect_layers = default_intersect_layers(expand = 0.1),
  annotations = list(geom_boxplot(aes(y = AvgRating))),
)

## ----upset-violin, fig.height=8-----------------------------------------------
simpleUpSet(
  movies, sets, n_intersect = 10,
  set_layers = default_set_layers(expand = 0.3),
  intersect_layers = default_intersect_layers(expand = 0.1),
  annotations = list(
    list(
      aes(y = AvgRating),
      geom_jitter(aes(colour = Decade), height = 0, width = 0.3, alpha = 0.5),
      geom_violin(fill = NA, quantiles = 0.5, quantile.linetype = 1),
      scale_colour_brewer(palette = "Paired"),
      guides(colour = guide_legend(nrow = 2, reverse = TRUE))
    )
  ), guides = "collect"
) &
  theme(legend.position = "bottom")

## ----session-info, echo = FALSE-----------------------------------------------
pander::pander(sessionInfo())

