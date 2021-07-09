library(tidyverse)

# x <- read_csv2("data/distance_matrix.csv")
# cidade_orig <- x[c(36, 95, 185, 354), ]$cidade_orig
# dist <- x %>%
#     dplyr::filter(cidade_orig %in% !!cidade_orig) %>%
#     dplyr::select(cidade_orig, tidyselect::all_of(cidade_orig))

# dist_m <- dist %>%
#     tibble::column_to_rownames("cidade_orig") %>% 
#     as.matrix()
# # colnames(dist_m) <- rownames(dist_m) <- stringr::str_replace(colnames(dist_m), "_Paraná", "")
# colnames(dist_m) <- rownames(dist_m) <- 1:ncol(dist_m)

# ============================================================================ #
#                                    TESTE                                     #
# ============================================================================ #

m <- matrix(
    c(
        Inf, 20, 30, 10, 11,
        15, Inf, 16, 4, 2,
        3, 5, Inf, 2, 4,
        19, 6, 18, Inf, 3,
        16, 4, 7, 16, Inf
    ),
    nrow = 5,
    byrow = TRUE
)

Rcpp::sourceCpp("pcv_naivy.cpp")
pcv_naivy(m)
 
Rcpp::sourceCpp("pcv_nearest_neighbor.cpp")
pcv_nearest_neighbor(m)
