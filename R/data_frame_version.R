library(tidyverse)

## --------------- Example (units, activations) data
##
## This will be a good candidate for an S4 class.

# activations range from 0 to 1
# resistances range from 1 to 0 (1 means activation flows perfectly, 0 means no flow; this is confusing; switch around?)
units <- tribble(
  ~name, ~activation, ~resistance,
  "cog1", 0.6, 1,
  "cog2", 0.5, 1,
  "cog3", 0.4, 1
)

# positive weights mean that activation in `source` will grow activation in `target`
relations <- tribble(
  ~source, ~target, ~weight,
  "cog1", "cog2", -0.3,
  "cog3", "cog2", -0.1,
  "cog1", "cog3", 1
)

## --------------- Define functions which act on (units, relations) pairs
##
## These will ultimately be methods of an S4 class.

get_next_activation <- function(unit_name, units, relations, floor = 0, ceiling = 1) {
  initial_activation <- units[units$name == unit_name,]$activation
  resistance <- units[units$name == unit_name,]$resistance
  
  inputs <- relations %>%
    filter(target == unit_name) %>%
    left_join(units, by = c("source" = "name"))
  
  net_input <- resistance * sum(inputs$weight * inputs$activation)

  new_activation <- if(net_input >= 0) {
    initial_activation + net_input * (ceiling - initial_activation)
  } else {
    initial_activation + net_input * (initial_activation - floor)
  }
  
  return(new_activation)
}

update_units <- function(units, relations, n = 1) {
  
  # choose which units will be updated in this cycle
  units_to_update <- character(0)
  while(length(units_to_update) < 1) {
    units_to_update <- sample(units$name, n)
  }
  
  # apply updating rules to those units
  new_activations <- tibble(
    name = units_to_update,
    new_activation = map_dbl(name, ~get_next_activation(., units, relations))
  )
  
  # return updated activations
  units %>%
    left_join(new_activations, by = "name") %>%
    mutate(activation = coalesce(new_activation, activation)) %>%
    select(-new_activation) %>%
    return()
}

consonance <- function(units, relations) {
  relations %>%
    left_join(select(units, name, activation), by = c("source" = "name")) %>%
    rename(source_activation = activation) %>%
    left_join(select(units, name, activation), by = c("target" = "name")) %>%
    rename(target_activation = activation) %>%
    mutate(partial_consonance = weight * source_activation * target_activation) %>%
    pull(partial_consonance) %>%
    sum()
}

dissonance_reduction_run <- function(units, relations) {
  activations <- list(units)
  consonances <- consonance(units, relations)
  diff_consonances <- Inf
  
  while(diff_consonances > 0.001) {
    activations <- c(activations, list(update_units(last(activations), relations)))
    consonances <- c(consonances, consonance(last(activations), relations))
    diff_consonances <- last(consonances) - nth(consonances, -2L)
  }
  
  activations %>%
    bind_rows(.id = "cycle") %>%
    mutate(cycle = as.integer(cycle) - 1L) %>%
    return()
}

## --------------- Run updating on example data

x <- dissonance_reduction_run(units, relations)

# good template for visualization of relative activation over time
ggplot(x, aes(x = cycle, y = activation, color = name)) +
  geom_point(size = 1.5) +
  geom_line(size = 1.5) +
  scale_x_continuous(breaks = min(x$cycle):max(x$cycle)) +
  scale_y_continuous(breaks = 0:10 * 0.1, labels = scales::percent_format(1)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    x = "Time",
    y = "Strength of Belief",
    color = "Cognitive Unit"
  )

# to do: visualizations of consonance over time
#
# possibly graph consonance below activation, but on its own scale
#   the scale will need to be fixed (not dependent on network)
#   and clearly communicate which values of consonance indicate a stable
#   set of beliefs and which indicate an unstable set
