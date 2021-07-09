library(tidyverse)

x <- read_csv2("data/distance_matrix.csv")
cidade_orig <- x[c(36, 95, 185, 354), ]$cidade_orig
dist <- x %>%
    dplyr::filter(cidade_orig %in% !!cidade_orig) %>%
    dplyr::select(cidade_orig, tidyselect::all_of(cidade_orig))

dist_m <- dist %>%
    tibble::column_to_rownames("cidade_orig") %>%
    as.matrix()

colnames(dist_m) <- rownames(dist_m) <- stringr::str_recidade(colnames(dist_m), "_Paraná", "")
x <- dist_m

# ============================================================================ #
#                            ALGORITIMO TENTATIVA                              #
# ============================================================================ #
# Matriz de distâncias
dd <- matrix(
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

compute_cost_r <- function(x) {
    if (all(x == Inf)) {
        return(list(minimum = 0, x = x))
    } else {
        minimum <- min(x)
        x <- x - minimum
        return(list(minimum = minimum, x = x))
    }
}

compute_matrix_cost_r <- function(m, from, to) {
    Nr <- c()
    Nc <- c()

    if (from != -1) {
        m[from, ] <- Inf
        m[, to] <- Inf
        m[to, from] <- Inf
    }

    # Compute cost rows
    for (i in 1:ncol(m)) {
        iter <- compute_cost_r(m[i, ])
        m[i, ] <- iter[[2]]
        Nr[i] <- iter[[1]]
    }

    # Compute cost cols
    for (i in 1:ncol(m)) {
        iter <- compute_cost_r(m[,i])
        m[ ,i] <- iter[[2]]
        Nc[i] <- iter[[1]]
    }

    list(
        custo = sum(Nc) + sum(Nr),
        cidade = to,
        matrix = m
    )
}

# compute_matrix_cost_r(dd, from = -1, to = 1)
compute_matrix_cost(dd, from = -1, to = 1)
dd

pcv_branch_and_bound <- function(m) {
    # Destinos possíveis
    int_cidades <- 2:ncol(m)

    branches <- list()
    branches <- append(branches, list(list(c(compute_matrix_cost(m, to = 1), caminho = 1))))

    # Custo minimo da iteração passada
    Ck_min_index <- 1
    lower <- branches[[1]][[Ck_min_index]]$custo
    while(TRUE) {
        branch_last_it <- tail(branches, 1)[[1]][[Ck_min_index]]
        branches_it <- list()
        # Para cada uma dos destinos disponiveis calcule o custo.
        for (i in int_cidades) {
            ck <- compute_matrix_cost(branch_last_it$m, from = branch_last_it$cidade, to = i)
            # Atualiza o custo: C_k + D(origem, destino) + C_{k-1}
            ck$custo <- ck$custo + branch_last_it$m[branch_last_it$cidade, i] + branch_last_it$custo
            ck$caminho <- c(branch_last_it$caminho, i)
            branches_it <- append(branches_it, list(ck))
        }

        branches <- append(branches, list(branches_it))

        Ck_min <- c()
        for (i in 1:length(branches_it)) {
            Ck_min <- c(Ck_min, branches_it[[i]]$custo)
        }
        # Seleciona a cidade com o menor custo.
        # Em caso de empate, a que estiver na primeira posição.
        Ck_min_index <- which(Ck_min == min(Ck_min))[1]
        
        # Remove cidade das possíveis rotas
        int_cidades <- int_cidades[!(int_cidades %in% branches_it[[Ck_min_index]]$cidade)]
        
        # Quando não tiver mais rotas para avaliar, pare.
        if (length(int_cidades) == 0) break
    }
    upper <- tail(branches, 1)[[1]][[1]]$custo

    rota_principal <- tail(branches, 1)[[1]][[1]]$caminho

    # Remove a rota principal do branches.
    branches_copy <- branches
    for (i in 2:length(branches_copy)) {
        remove_rota_principal <- c()
        for (j in 1:length(branches_copy[[i]])) {
            if (branches_copy[[i]][[j]][["caminho"]][i] == rota_principal[i]) {
                remove_rota_principal <- c(remove_rota_principal, j)
            }
        }
        branches_copy[[i]] <- branches_copy[[i]][-remove_rota_principal]
    }

    # Procura por novas rotas onde o custo está entre o lower e upper.
    novas_rotas <- list()
    for (i in 2:(length(branches_copy)-1)) {
        branch_it <- branches_copy[[i]]

        for (j in 1:length(branch_it)) {
            custo_it <- branch_it[[j]][["custo"]]
            if (custo_it > lower & custo_it < upper) {
                novas_rotas <- append(novas_rotas, list(branch_it[[j]][["caminho"]]))
            }
        }
    }

    custo_soma <- tail(branches, 1)[[1]][[1]][["matrix"]][tail(branches, 1)[[1]][[1]][["cidade"]], 1]
    custo_final <- tail(branches, 1)[[1]][[1]][["custo"]] + custo_soma
    caminho_final <- paste0(c(tail(branches, 1)[[1]][[1]][["caminho"]], 1), collapse = "-")
    result <- data.frame(
        caminho = caminho_final,
        custo = custo_final
    )

    # Se existir novas rotas.
    if (length(novas_rotas) > 0) {
        branches_novas_rotas <- list()
        for (i in 1:length(novas_rotas)) {
            int_cidades <- 2:ncol(m)
            int_cidades <- int_cidades[!(int_cidades %in% novas_rotas[[i]])]

            # Nó
            k <- length(novas_rotas[[i]])
            branch_explora <- branches_copy[1:k]
            branches_it <- tail(branch_explora, 1)[[1]]

            Ck_min <- c()
            for (i in 1:length(branches_it)) {
                Ck_min <- c(Ck_min, branches_it[[i]]$custo)
            }
            # Seleciona a cidade com o menor custo.
            # Em caso de empate, a que estiver na primeira posição.
            Ck_min_index <- which(Ck_min == min(Ck_min))[1]

            while (TRUE) {
                branch_last_it <- tail(branch_explora, 1)[[1]][[Ck_min_index]]
                branches_it <- list()
                # Para cada uma dos destinos disponiveis calcule o custo.
                for (i in int_cidades) {
                    ck <- compute_matrix_cost(branch_last_it$m, from = branch_last_it$cidade, to = i)
                    # Atualiza o custo: C_k + D(origem, destino) + C_{k-1}
                    ck$custo <- ck$custo + branch_last_it$m[branch_last_it$cidade, i] + branch_last_it$custo
                    ck$caminho <- c(branch_last_it$caminho, i)
                    branches_it <- append(branches_it, list(ck))
                }

                branch_explora <- append(branch_explora, list(branches_it))

                Ck_min <- c()
                for (i in 1:length(branches_it)) {
                    Ck_min <- c(Ck_min, branches_it[[i]]$custo)
                }
                # Seleciona a cidade com o menor custo.
                # Em caso de empate, a que estiver na primeira posição.
                Ck_min_index <- which(Ck_min == min(Ck_min))[1]

                # Remove cidade das possíveis rotas
                int_cidades <- int_cidades[!(int_cidades %in% branches_it[[Ck_min_index]]$cidade)]

                custo_it <- Ck_min[Ck_min_index]

                if (custo_it > upper) {
                    break
                }

                # Quando não tiver mais rotas para avaliar, pare.
                if (length(int_cidades) == 0) {
                    branches_novas_rotas <- append(
                        branches_novas_rotas,
                        list(branch_explora)
                    )
                    break
                }
            }
        }

        if (length(branches_novas_rotas) > 0) {
            for (i in 1:length(branches_novas_rotas)) {
                result_novas_rotas <- branches_novas_rotas[[i]]
                custo_soma <- tail(result_novas_rotas, 1)[[1]][[1]][["matrix"]][tail(result_novas_rotas, 1)[[1]][[1]][["cidade"]], 1]
                custo_final <- tail(result_novas_rotas, 1)[[1]][[1]][["custo"]] + custo_soma
                caminho_final <- paste0(c(tail(result_novas_rotas, 1)[[1]][[1]][["caminho"]], 1), collapse = "-")
                result_novo <- data.frame(
                    caminho = caminho_final,
                    custo = custo_final
                )

                result <- rbind(result, result_novo)
            }
        }
    }

    return(result[result$custo == min(result$custo), ])
}
