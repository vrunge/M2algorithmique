
#' Needleman–Wunsch Global Sequence Alignment
#'
#' Computes the optimal global alignment between two sequences using the
#' Needleman–Wunsch algorithm with a simple scoring scheme.
#'
#' @param seq1 A character string representing the first sequence (e.g., DNA, RNA, or protein).
#' @param seq2 A character string representing the second sequence.
#' @param match Numeric score for a match between two characters. Default is 1.
#' @param mismatch Numeric penalty for a mismatch between two characters. Default is -1.
#' @param gap Numeric penalty for a gap (insertion or deletion). Default is -2.
#'
#' @return A list containing:
#' \describe{
#'   \item{score}{The optimal alignment score (numeric).}
#'   \item{score_matrix}{The dynamic programming score matrix (matrix).}
#'   \item{traceback_matrix}{The matrix storing traceback directions ("diag", "up", "left").}
#'   \item{alignment}{A character vector of length 2 containing the aligned sequences.}
#' }
#'
#' @details
#' The function implements the Needleman–Wunsch algorithm:
#' \enumerate{
#'   \item Initializes a scoring matrix with gap penalties.
#'   \item Fills the matrix using dynamic programming to compute optimal alignment scores.
#'   \item Stores traceback directions in a separate matrix to reconstruct the alignment.
#'   \item Performs a traceback from the bottom-right corner to recover the optimal alignment.
#' }
#' Time and space complexity are both O(n*m), where n and m are the lengths of seq1 and seq2.
#'
#' @examples
#' res <- needleman_wunsch("GATTACA", "GATAAA")
#' res$score
#' res$alignment
#' res$score_matrix
#' res$traceback_matrix
#'
#' @export
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
      
      best <- max(c(diag_score, up_score, left_score))
      score[i, j] <- best
      
      # Store the move that gave the best score 
      # (define the priorities in case of equal scores)
      arg_best <- which.max(c(diag_score, up_score, left_score))
      traceback[i, j] <- c("diag", "up", "left")[arg_best]
    }
  }
  
  # Traceback phase (backtracking step)
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
    } else if (move == "up")
    {
      alignment1 <- paste0(s1[i - 1], alignment1)
      alignment2 <- paste0("-", alignment2)
      i <- i - 1
    } else if (move == "left")
    {
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
