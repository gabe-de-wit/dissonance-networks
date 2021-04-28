library(tidyverse)

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
  new_activations <- tibble(
    name = sample(units$name, n),
    new_activation = map_dbl(name, ~get_next_activation(., units, relations))
  )
  
  units %>%
    left_join(new_activations, by = "name") %>%
    mutate(activation = coalesce(new_activation, activation)) %>%
    select(-new_activation) %>%
    return()
}

# pipe type mechanism is appealing for repeated updating
units %>%
  update_units(relations) %>%
  update_units(relations) %>%
  update_units(relations) %>%
  update_units(relations) %>%
  update_units(relations)
