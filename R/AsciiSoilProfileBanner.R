
asciiProfile <- function(){

  # Define horizon styles using explicit crayon:: calls
  col_o    <- crayon::make_style("#1E6319") # Bright Neon Green
  col_a    <- crayon::make_style("#572503") # Bright Yellow
  col_e    <- crayon::make_style("#8F5704") # Pure White
  col_b    <- crayon::make_style("#FF3300") # Bright Red / Orange
  col_c    <- crayon::make_style("#1A0DDE") # Electric Cyan
  col_r    <- crayon::make_style("#EE00FF") # Vivid Magenta
  col_logo <- crayon::red                  # Bright Red for ANSIS Logo

  # Visual texture ASCII character definitions
  sym_o1 <- "*"
  sym_o2 <- "%"
  sym_a  <- "^"
  sym_e  <- "o"
  sym_b  <- "#"
  sym_c1 <- "~"
  sym_c2 <- "/"
  sym_r  <- "="

  cat("\n\n")

  # Construct texture patterns from defined character variables
  pat_o_1 <- paste(rep(sym_o1, 12), collapse = " ")
  pat_o_2 <- sprintf(" %s  %s  %s  %s  %s  %s  %s  %s", sym_o1, sym_o2, sym_o1, sym_o2, sym_o1, sym_o2, sym_o1, sym_o2)

  pat_a_1 <- paste(rep(sym_a, 12), collapse = " ")
  pat_a_2 <- sprintf(" %s ", paste(rep(sym_a, 11), collapse = " "))

  pat_e_1 <- paste(rep(sym_e, 12), collapse = " ")
  pat_e_2 <- sprintf(" %s ", paste(rep(sym_e, 11), collapse = " "))

  pat_b_1 <- paste(rep(sym_b, 12), collapse = " ")
  pat_b_2 <- sprintf(" %s ", paste(rep(sym_b, 11), collapse = " "))

  pat_c_1 <- paste(rep(sym_c1, 12), collapse = " ")
  pat_c_2 <- sprintf(" %s %s %s  %s %s %s  %s %s %s %s", sym_c2, sym_c2, sym_c2, sym_c1, sym_c1, sym_c1, sym_c2, sym_c2, sym_c2, sym_c1)
  pat_c_3 <- sprintf("%s %s %s  %s %s %s  %s %s %s %s %s", sym_c1, sym_c1, sym_c1, sym_c2, sym_c2, sym_c2, sym_c1, sym_c1, sym_c1, sym_c2, sym_c2)

  pat_r_1 <- paste(rep(sym_r, 23), collapse = "")
  pat_r_2 <- sprintf(" %s", paste(rep(sym_r, 21), collapse = ""))

  gap <- "               "


 # cat("========================================================================================\n")

  # O Horizon
  cat("  0 --     +-------+ ", col_o(pat_o_1), "\n")
  cat("           |   O   | ", col_o(pat_o_2), "\n")

  # A Horizon
  cat("  5 --     +-------+ ", col_a(pat_a_1), "\n")
  cat("           |       | ", col_a(pat_a_2), gap, col_logo("  AAAAAA    NN    NN   SSSSSS   IIIIII   SSSSSS  "), "\n")
  cat("           |   A   | ", col_a(pat_a_1), gap, col_logo(" AA    AA   NNN   NN  SS    SS    II    SS    SS "), "\n")

  # E Horizon
  cat(" 25 --     +-------+ ", col_e(pat_e_1), gap, col_logo(" AA    AA   NNNN  NN  SS          II    SS       "), "\n")
  cat("           |   B1  | ", col_e(pat_e_2), gap, col_logo(" AAAAAAAA   NN NN NN   SSSSSS     II     SSSSSS  "), "\n")
  cat("           |       | ", col_e(pat_e_1), gap, col_logo(" AA    AA   NN  NNNN        SS    II          SS "), "\n")

  # B Horizon
  cat(" 45 --     +-------+ ", col_b(pat_b_1), gap, col_logo(" AA    AA   NN   NNN  SS    SS    II    SS    SS "), "\n")
  cat("           |       | ", col_b(pat_b_2), gap, col_logo(" AA    AA   NN    NN   SSSSSS   IIIIII   SSSSSS  "), "\n")
  cat("           |   B2  | ", col_b(pat_b_1), "\n")

  # C Horizon
  cat(" 80 --     +-------+ ", col_c(pat_c_1), "\n")
  cat("           |   C   | ", col_c(pat_c_2), "\n")
  cat("           |       | ", col_c(pat_c_3), "\n")

  # # R Horizon
  # cat("120+ -     +-------+ ", col_r(pat_r_1), "\n")
  # cat("           |   R   | ", col_r(pat_r_2), "\n")
  # cat("           +-------+ ", col_r(pat_r_1), "\n")


}
