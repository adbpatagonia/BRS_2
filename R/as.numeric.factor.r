# ADB, ERL Vancouver

# Function to transform a facotr with numeric level (i.e. db level) to numeric

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
