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

pcv_nearest_neighbor <- function(m) {
    
    unvisited <- 1:ncol(m)
    unvisited <- unvisited[-1]
    caminho <- c(1)
    custo <- 0

    while (TRUE) {
        i <- tail(caminho, 1)
        destino <- m[i, ]
        destino[-unvisited] <- Inf
        cidade_destino <- which.min(destino)
        custo_destino <- m[i, cidade_destino]

        caminho <- c(caminho, cidade_destino)
        custo <- custo + custo_destino

        unvisited = unvisited[unvisited != cidade_destino]
        if (length(unvisited) == 0) break
    }

    # Caminho de volta para a cidade origem
    i <- tail(caminho, 1)
    custo_destino <- m[i, 1]
    caminho <- c(caminho, 1)
    custo <- custo + custo_destino


    df <- data.frame(
        caminho = paste0(caminho, collapse = "-"),
        custo = custo
    )

    return(df)
}