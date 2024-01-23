library(poof)
library(tidyverse)
library(tictoc)
# devtools::install_git("https://github.com/hzambran/hydroTSM.git")
# devtools::install_git("https://github.com/hzambran/hydroGOF.git")
library(hydroGOF)
library(data.table)
library(furrr)
library(tictoc)

# random numbers

tic()
for(n in 1:8) {
  assign(paste0("n", 10, n), rnorm(10^n))
}
toc()
                                                                                                                                                                                    
cores <- 8
tic()
plan(multicore, workers = cores)
# Set the number of iterations
iterations <- 1:8

# Use future_map to parallelize the creation of random numbers
future_walk(iterations, ~{
  n <- 10^.
  set.seed(123)  # Set seed for reproducibility
  assign(paste0("n", n), rnorm(n), envir = .GlobalEnv)
})

plan(sequential)
toc()




# Create a data.table called 'bench'
bench <- data.table(id = 1:8)

# Generate n vectors of random numbers
vectors <- lapply(1:n, function(i) rnorm(10^i))
# Function to generate random vectors and calculate mean time

computeMeanTime <- function(n, fun) {
  
  # Measure time to compute mean for each vector
  times <- numeric(n)
  for (i in 1:n) {
    tic()
    mean_value <- fun(vectors[[i]], vectors[[i]])
    times[i] <- toc()$callback_msg
  }
  
  return(times[length(times)])  # Return the last item of the vector
}

# Apply the function to each row of 'bench'
bench[, paste0("poof_iter", 1:10) := replicate(10, 
                                        computeMeanTime(id, 
                                                        fun = poof::KGE), 
                                        simplify = FALSE), 
      by = id]

bench[, paste0("hgf_iter", 1:10) := replicate(10, 
                                          computeMeanTime(id, 
                                                          fun = hydroGOF::KGE), 
                                          simplify = FALSE), 
      by = id]

res <- bench |> 
  pivot_longer(cols = -id) |> 
  mutate(value = as.numeric(gsub(x = value, pattern = " sec elapsed", replacement = ""))) |> 
  separate(col = name, into = c("algorithm", "iteration"), sep = "_") |> 
  mutate(iteration = gsub(x = iteration, pattern = "iter", replacement = ""), 
         id = as.factor(id), 
         iteration = factor(iteration, levels = 1:10))

res |> 
  ggplot() +
  geom_boxplot(aes(color = algorithm, x = value)) +
  facet_wrap(~id) +
  scale_x_time(minor_breaks = "0.1 second") +
  scale_x_log10() +
  theme_minimal() +
  scale_color_viridis_d()
res


