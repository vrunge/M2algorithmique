

seq1 <- "GATTACA"
seq2 <- "GATTACA"

res <- needleman_wunsch(seq1, seq2, match = 1, mismatch = -1, gap = -2)

cat("Alignment score:", res$score, "\n\n")

res$alignment[1]
res$alignment[2]

# Display scoring matrix (optional)
res$score_matrix

res$traceback_matrix




res <- needleman_wunsch("GATTACA", "GATCA")
res$score
res$score_matrix
res$traceback_matrix
res$alignment






# ---------------------------------------------
# Empirical test of Needleman–Wunsch complexity
# ---------------------------------------------

# Load the function (assume it's already defined)
# source("needleman_wunsch.R")  # Uncomment if stored externally

# Function to generate a random DNA sequence of given length
random_seq <- function(n)
{
  paste0(sample(c("A", "C", "G", "T"), size = n, replace = TRUE), collapse = "")
}

# Function to measure runtime for a given sequence length
measure_time <- function(n) {
  seq1 <- random_seq(n)
  seq2 <- random_seq(n)
  t <- system.time(needleman_wunsch(seq1, seq2))[3]  # elapsed time
  return(t)
}

# Test on increasing sequence lengths
sizes <- c(100, 200, 400, 800, 1600)
times <- sapply(sizes, measure_time)

# Display results
data <- data.frame(size = sizes, time = times)
print(data)

# Plot runtime vs. size on log–log scale
plot(data$size, data$time,
     log = "xy", type = "p", pch = 19, col = "blue",
     xlab = "Sequence length (n)",
     ylab = "Runtime (seconds)",
     main = "Needleman–Wunsch empirical complexity")


# Compute the regression: log(time) = a + b * log(size)
fit <- lm(log(data$time) ~ log(data$size))
coef_fit <- coef(fit)
slope <- coef_fit[2]
intercept <- coef_fit[1]

# Add regression line
pred_times <- exp(predict(fit))
lines(data$size, pred_times, col = "darkgreen", lwd = 2)

# Add text with regression result
text(x = min(data$size) * 1.1,
     y = max(data$time) / 1.5,
     labels = paste0("log(time) = ", intercept, " + ", slope, " * log(n)\n",
                     "time = cst n^", slope),
     col = "darkgreen", adj = 0, cex = 0.9)

# Add legend
legend("bottomright",
       legend = c("Measured times", "Log–log regression"),
       col = c("blue", "darkgreen"),
       lty = c(1, 1), pch = c(19, NA, NA), bty = "n")




#####




res <- needleman_wunsch("GATTACA", "GATCA")
res$score
res$score_matrix
res$traceback_matrix
res$alignment

res <- needleman_wunsch_cpp("GATTACA", "GATCA")
res$score
res$score_matrix
res$traceback_matrix
res$alignment



n <- 5000
seq1 <- random_seq(n)
seq2 <- random_seq(n)
t1 <- system.time(needleman_wunsch(seq1, seq2))[3]  
t2 <- system.time(needleman_wunsch_cpp(seq1, seq2))[3]  
t1
t2
t1/t2










###############################



# ---------------------------------------------
# Empirical test of Needleman–Wunsch complexity
# ---------------------------------------------

# Load the function (assume it's already defined)
# source("needleman_wunsch.R")  # Uncomment if stored externally

# Function to generate a random DNA sequence of given length
random_seq <- function(n)
{
  paste0(sample(c("A", "C", "G", "T"), size = n, replace = TRUE), collapse = "")
}

# Function to measure runtime for a given sequence length
measure_time <- function(n) {
  seq1 <- random_seq(n)
  seq2 <- random_seq(n)
  t <- system.time(needleman_wunsch_cpp(seq1, seq2))[3]  # elapsed time
  return(t)
}

# Test on increasing sequence lengths
sizes <- c(100, 200, 400, 800, 1600)*10
times <- sapply(sizes, measure_time)

# Display results
data <- data.frame(size = sizes, time = times)
print(data)

# Plot runtime vs. size on log–log scale
plot(data$size, data$time,
     log = "xy", type = "p", pch = 19, col = "blue",
     xlab = "Sequence length (n)",
     ylab = "Runtime (seconds)",
     main = "Needleman–Wunsch empirical complexity")


# Compute the regression: log(time) = a + b * log(size)
fit <- lm(log(data$time) ~ log(data$size))
coef_fit <- coef(fit)
slope <- coef_fit[2]
intercept <- coef_fit[1]

# Add regression line
pred_times <- exp(predict(fit))
lines(data$size, pred_times, col = "darkgreen", lwd = 2)

# Add text with regression result
text(x = min(data$size) * 1.1,
     y = max(data$time) / 1.5,
     labels = paste0("log(time) = ", intercept, " + ", slope, " * log(n)\n",
                     "time = cst n^", slope),
     col = "darkgreen", adj = 0, cex = 0.9)

# Add legend
legend("bottomright",
       legend = c("Measured times", "Log–log regression"),
       col = c("blue", "darkgreen"),
       lty = c(1, 1), pch = c(19, NA, NA), bty = "n")




#####





seq1 <- "AACGTGACGCT"
seq2 <- "AGACCTAGAAA"
res <- needleman_wunsch(seq1, seq2, match = 1, mismatch = -1, gap = 2)
cat("Alignment score:", res$score, "\n\n")
res$score_matrix
res$traceback_matrix
res$alignment[1]
res$alignment[2]



