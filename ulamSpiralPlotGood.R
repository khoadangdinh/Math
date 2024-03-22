rm(list = ls())
library(ggplot2)

# Function to generate Ulam Spiral
# some info here: 
# https://en.wikipedia.org/wiki/Ulam_spiral
# Function to check if a number is prime
is_prime <- function(n) {
  if (n <= 1) {
    return(FALSE)
  }
  
  for (i in 2:sqrt(n)) {
    if (n %% i == 0) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}


#is_prime(13)

ulam_spiral <- function(n) {
  # Initialize variables
  x <- y <- 0
  isPrime<-FALSE
  
  dx <- 0
  dy <- -1
  spiral_data <- data.frame(x = numeric(n), y = numeric(n),P = rep(FALSE,n))
  
  # Fill in spiral_data with Ulam Spiral coordinates
  for (i in 1:n) {
    
    spiral_data[i, ] <- c(x, y,is_prime(i))
    
    # Change direction when reaching a perfect square or turning right twice
    if ((x == y) || ((x < 0) && (x == -y)) || ((x > 0) && (x == 1 - y))) {
      temp <- dx
      dx <- -dy
      dy <- temp
    }
    
    x <- x + dx
    y <- y + dy
  }
  
  return(spiral_data)
}

# Set the number of points (adjust as needed)
num_points <- 10000

# Create Ulam Spiral data
spiral_data <- ulam_spiral(num_points)


# Plot Ulam Spiral
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


ulam_spiral_plot<-ggplot(spiral_data, aes(x, y,color=as.character(P))) +
  geom_point(size = 2,shape=15) +
  scale_fill_manual(values=cbbPalette)+
  ggtitle("Ulam Spiral")+
  theme_minimal()+
  coord_fixed() +
  theme(legend.position = "none")+
  theme(plot.title = element_text(vjust=-1,hjust = 0.5,family = "Times", face = "bold", size = 40))
  
  ulam_spiral_plot
#png("ulamSpiral.png",width = 1000, height = 1000,units = "px")
#ulam_spiral_plot
#dev.off()

