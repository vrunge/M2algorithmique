#include <Rcpp.h>
using namespace Rcpp;

//' Needleman–Wunsch Global Sequence Alignment (Rcpp version)
//'
//' Computes the optimal global alignment between two sequences using the
//' Needleman–Wunsch algorithm with a simple scoring scheme.
//'
//' @param seq1 A character string representing the first sequence (e.g., DNA, RNA, or protein).
//' @param seq2 A character string representing the second sequence.
//' @param match Numeric score for a match between two characters. Default is 1.
//' @param mismatch Numeric penalty for a mismatch between two characters. Default is -1.
//' @param gap Numeric penalty for a gap (insertion or deletion). Default is -2.
//'
//' @return A list containing:
//' \describe{
//'   \item{score}{The optimal alignment score (numeric).}
//'   \item{score_matrix}{The dynamic programming score matrix (numeric matrix).}
//'   \item{traceback_matrix}{The matrix storing traceback directions ("diag", "up", "left").}
//'   \item{alignment}{A character vector of length 2 containing the aligned sequences.}
//' }
//'
//' @details
//' The Needleman–Wunsch algorithm initializes a scoring matrix with gap penalties,
//' fills it via dynamic programming, and performs a traceback to recover the
//' optimal global alignment. Time and space complexity are both O(n*m).
//'
//' @examples
//' res <- needleman_wunsch_cpp("GATTACA", "GATAAA")
//' res$score
//' res$alignment
//' @export
// [[Rcpp::export]]
List needleman_wunsch_cpp(std::string seq1,
                           std::string seq2,
                           double match = 1,
                           double mismatch = -1,
                           double gap = -2)
{
   int n = seq1.size();
   int m = seq2.size();
   
   NumericMatrix score(n + 1, m + 1);
   CharacterMatrix traceback(n + 1, m + 1);
   
   
   // Initialize first row and column
   for (int i = 1; i <= n; ++i)
    {
     score(i, 0) = i * gap;
     traceback(i, 0) = "up";
   }
   for (int j = 1; j <= m; ++j)
    {
     score(0, j) = j * gap;
     traceback(0, j) = "left";
   }
   
   double diag_score;
   double up_score;
   double left_score;
   double best;
   std::string move;
   
   // Fill DP table
   for (int i = 1; i <= n; ++i)
    {
     for (int j = 1; j <= m; ++j)
    {
      diag_score = score(i - 1, j - 1) + (seq1[i - 1] == seq2[j - 1] ? match : mismatch);
      up_score   = score(i - 1, j) + gap;
      left_score = score(i, j - 1) + gap;
       
       // Choose best score
      best = diag_score;
      move = "diag";
       
       if (up_score > best)
        {
         best = up_score;
         move = "up";
       }
       if (left_score > best)
        {
         best = left_score;
         move = "left";
       }
       score(i, j) = best;
       traceback(i, j) = move;
     }
   }
   
   // Traceback (Backtracking step)
   std::string align1 = "";
   std::string align2 = "";
   int i = n;
   int j = m;
   
   while (i > 0 || j > 0)
    {
     move = as<std::string>(traceback(i, j));
     if (move == "diag") {
       align1 = seq1[i - 1] + align1;
       align2 = seq2[j - 1] + align2;
       i--; j--;
     } else if (move == "up") {
       align1 = seq1[i - 1] + align1;
       align2 = "-" + align2;
       i--;
     } else if (move == "left") {
       align1 = "-" + align1;
       align2 = seq2[j - 1] + align2;
       j--;
     } else {
       break;
     }
   }
   
   return List::create(
     _["score"] = score(n, m),
     _["score_matrix"] = score,
     _["traceback_matrix"] = traceback,
     _["alignment"] = CharacterVector::create(align1, align2)
   );
}
 