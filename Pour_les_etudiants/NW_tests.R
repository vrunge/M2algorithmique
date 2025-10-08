

seq1 <- "GATTACA"
seq2 <- "GCATGCU"

res <- needleman_wunsch(seq1, seq2, match = 1, mismatch = -1, gap = -2)

cat("Alignment score:", res$score, "\n\n")

cat(res$alignment[1], "\n", res$alignment[2], "\n")

# Display scoring matrix (optional)
res$matrix



seq1 <- "GATTACA"
seq2 <- "GATTACA"

res <- needleman_wunsch(seq1, seq2, match = 1, mismatch = -1, gap = -2)

cat("Alignment score:", res$score, "\n\n")

res$alignment[1]
res$alignment[2]

# Display scoring matrix (optional)
res$score_matrix

res$traceback_matrix
