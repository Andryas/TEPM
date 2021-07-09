// [[depends(RcppEigen)]]
// [[depends(RcppNumerical)]]

#include <Rcpp.h>

using namespace Rcpp;

//[[Rcpp::export]]
List compute_cost(NumericVector x) {
    LogicalVector v = (x == R_PosInf);
    if (is_true(all(v)))
    {
        return List::create(Named("minimo") = 0, Named("x") = x);
    }
    else
    {
        double minimo = min(x);
        NumericVector x2 = x - minimo;
        return List::create(Named("minimo") = minimo, Named("x") = x2);
    }
}

//[[Rcpp::export]]
List compute_matrix_cost(NumericMatrix x, int from, int to) {
    // https://stackoverflow.com/questions/31015321/rcpp-how-to-ensure-deep-copy-of-a-numericmatrix
    NumericMatrix m(clone(x));
    NumericVector Nr;
    NumericVector Nc;
    NumericVector infinito = NumericVector(m.ncol() - 1, R_PosInf);

    if (from != -1)
    {
        m(to, from) = R_PosInf;
        m( from, _ ) = infinito;
        m( _ , to) = infinito;
    }

    // Custo por linha
    for (int i = 0; i < (m.ncol()); i++)
    {
        NumericVector linha = m(i, _);
        List iter = compute_cost(linha);

        NumericVector nova_linha = iter["x"];
        double minimo = iter["minimo"];
        m(i, _) = nova_linha;
        Nr.push_back(minimo);
    }

    // Custo por linha
    for (int i = 0; i < (m.ncol()); i++)
    {
        NumericVector coluna = m( _ , i);
        List iter = compute_cost(coluna);

        NumericVector nova_coluna = iter["x"];
        double minimo = iter["minimo"];
        m(i, _) = nova_coluna;
        Nc.push_back(minimo);
    }
    
    double custo_total_linha = sum(Nr);
    double custo_total_coluna = sum(Nc);

    return List::create(
        Named("custo") = custo_total_linha + custo_total_coluna,
        Named("cidade") = to,
        Named("matrix") = m);
}

//[[Rcpp::export]]
DataFrame pcv_branch_and_bound(NumericMatrix x)
{
    // Vetor de inteiros para se referenciar a cada cidade
    NumericMatrix m(clone(x));
    IntegerVector int_cidades = seq_len(m.ncol() - 1);
    int_cidades.erase(0);

    List branches;
    
    // branches <- append(branches, list(list(c(compute_matrix_cost(m, to = 1), caminho = 1))))

    // # Custo minimo da iteração passada
    // Ck_min_index <- 1
    // lower <- branches[[1]][[Ck_min_index]]$custo         
    // while(TRUE) {
    //     branch_last_it <- tail(branches, 1)[[1]][[Ck_min_index]]
    //     branches_it <- list()
    //     # Para cada uma dos destinos disponiveis calcule o custo.
    //     for (i in int_cidades) {
    //         ck <- compute_matrix_cost(branch_last_it$m, from = branch_last_it$cidade, to = i)
    //         # Atualiza o custo: C_k + D(origem, destino) + C_{k-1}
    //         ck$custo <- ck$custo + branch_last_it$m[branch_last_it$cidade, i] + branch_last_it$custo
    //         ck$caminho <- c(branch_last_it$caminho, i)
    //         branches_it <- append(branches_it, list(ck))
    //     }

    //     branches <- append(branches, list(branches_it))

    //     Ck_min <- c()
    //     for (i in 1:length(branches_it)) {
    //         Ck_min <- c(Ck_min, branches_it[[i]]$custo)
    //     }
    //     # Seleciona a cidade com o menor custo.
    //     # Em caso de empate, a que estiver na primeira posição.
    //     Ck_min_index <- which(Ck_min == min(Ck_min))[1]
        
    //     # Remove cidade das possíveis rotas
    //     int_cidades <- int_cidades[!(int_cidades %in% branches_it[[Ck_min_index]]$cidade)]
        
    //     # Quando não tiver mais rotas para avaliar, pare.
    //     if (length(int_cidades) == 0) break
    // }
    // upper <- tail(branches, 1)[[1]][[1]]$custo

    // rota_principal <- tail(branches, 1)[[1]][[1]]$caminho
}