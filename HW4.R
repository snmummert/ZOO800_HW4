####################################
########### HW WEEK 4 #############
#Sophia Mummert and Maddie Thall#####
#####################################

###Obj 1###
library(tidyverse)
library(palmerpenguins)
penguins = penguins

#Create function
binary_conversion = function(x, breakpoint, low_label, high_label) {
  ifelse(x <= breakpoint, low_label, high_label)
}

#Set parameters
breakpoint = 4200
x = penguins$body_mass_g

#Run function
penguin_binary = binary_conversion(x, breakpoint, "Small", "Large")

###Obj 2###

#Create function
penguin_bodymass_size = function(x, breakpoints, labels) {
  cut(x, 
      breaks = c(-Inf, breakpoints, Inf),
      labels = labels,
      right = TRUE
      )
}

#Set parametes
breakpoints = c(3500, 4500)
labels = c("small", "medium", "large")

#Run function
penguin_bodymass_size(x, breakpoints, labels)

###Obj 3###

#show how many species
table(penguins$species)

#use quantile to get breakpoints for each species
adelie = subset(penguins, species == "Adelie")
gentoo = subset(penguins, species == "Gentoo")
chinstrap = subset(penguins, species == "Chinstrap")

#Using tidyverse because its easier to set the dataframe this way
library(dplyr)
breakpoints_list = penguins %>%
  group_by(species) %>%
  summarise(
    q33 = quantile(body_mass_g, .33, na.rm = TRUE),
    q66 = quantile(body_mass_g, .66, na.rm = TRUE)
  )

#Create function: I'm going to be honest with you, we worked on this for 1.5 hours
#and had no luck, the we were having so many error messages that just did not help 
#us get to the bottom of what was going on. Our function looked like this in a lot of ways
#but it was missing several things like cat_vec, brk set as numeric, and brackets in the right place
#anyway, ultimatly we had to use ChatGPT to help us with this one
species_body_size <- function(x, species, breakpoints_list, labels) {
  cat_vec <- rep(NA, length(x))
  for (sp in unique(species)) {
    idx <- which(species == sp)
    brk <- as.numeric(breakpoints_list[breakpoints_list$species == sp, c("q33", "q66")])
    cat_vec[idx] <- as.character(cut(x[idx], c(-Inf, brk, Inf), labels = labels, right = TRUE))
  }
  factor(cat_vec, levels = labels)
}

#Run function
species_body_size_results <- species_body_size(
  x = x, 
  species = penguins$species,
  breakpoints_list = breakpoints_list,
  labels = labels
)

penguins$body_mass_cat <- species_body_size_results

###Obj 4###


library(ggplot2)
ggplot(data = penguins, aes(x = body_mass_cat, y = body_mass_g, fill = species)) +
  geom_boxplot() +
  facet_wrap(~species) +
  labs(
    title = "Penguin Body Mass by Species and Size Category",
    x = "Size",
    y = "Body Mass (g)"
  )

