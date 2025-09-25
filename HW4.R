####################################
########### HW WEEK 4 #############
#Sophia Mummert and Maddie Thall#####
#####################################

###Obj 1###
library(palmerpenguins)
penguins = penguins

#Create function
binary_conversion = function(x, breakpoint, low_label, high_label) {
  ifelse(x <= breakpoint, low_label, high_label)
}

#Set perameters
breakpoint = 4200
x = penguins$body_mass_g

#Run function
penguin_binary = binary_conversion(x, breakpoint, "Small", "Large")

###Obj 2###
penguin_size_range = 
