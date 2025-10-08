


needleman_wunsch <- function(seq1, 
                             seq2, 
                             match = 1, 
                             mismatch = -1, 
                             gap = -2) 
{
  
  # Convert sequences to vectors of characters
  s1 <- unlist(strsplit(seq1, split = ""))
  s2 <- unlist(strsplit(seq2, split = ""))
  n <- length(s1)
  m <- length(s2)
  
  # Initialize score and traceback matrices
  score <- matrix(0, nrow = n + 1, ncol = m + 1)
  traceback <- matrix("", nrow = n + 1, ncol = m + 1)
  
  # Fill first row and column with gap penalties
  for (i in 2:(n + 1))
  {
    score[i, 1] <- (i - 1) * gap
    traceback[i, 1] <- "up"   # from above (gap in seq2)
  }
  for (j in 2:(m + 1))
  {
    score[1, j] <- (j - 1) * gap
    traceback[1, j] <- "left" # from left (gap in seq1)
  }
  
  # Fill the dynamic programming table
  for (i in 2:(n + 1)) 
  {
    for (j in 2:(m + 1))
    {
      diag_score <- score[i - 1, j - 1] + ifelse(s1[i - 1] == s2[j - 1], match, mismatch)
      up_score   <- score[i - 1, j] + gap
      left_score <- score[i, j - 1] + gap
      
      best <- max(diag_score, up_score, left_score)
      score[i, j] <- best
      
      # Store the move that gave the best score
      if (best == diag_score) {
        traceback[i, j] <- "diag"
      } else if (best == up_score) {
        traceback[i, j] <- "up"
      } else {
        traceback[i, j] <- "left"
      }
    }
  }
  
  # Traceback phase
  alignment1 <- ""
  alignment2 <- ""
  i <- n + 1
  j <- m + 1
  
  while (i > 1 || j > 1) 
  {
    move <- traceback[i, j]
    
    if (move == "diag")
    {
      alignment1 <- paste0(s1[i - 1], alignment1)
      alignment2 <- paste0(s2[j - 1], alignment2)
      i <- i - 1
      j <- j - 1
    } else if (move == "up") {
      alignment1 <- paste0(s1[i - 1], alignment1)
      alignment2 <- paste0("-", alignment2)
      i <- i - 1
    } else if (move == "left") {
      alignment1 <- paste0("-", alignment1)
      alignment2 <- paste0(s2[j - 1], alignment2)
      j <- j - 1
    } else {
      break
    }
  }
  
  return(list(
    score = score[n + 1, m + 1],
    score_matrix = score,
    traceback_matrix = traceback,
    alignment = c(alignment1, alignment2)
  ))
}
