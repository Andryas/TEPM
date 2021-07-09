// [[depends(RcppEigen)]]
// [[depends(RcppNumerical)]]

#include <Rcpp.h>

using namespace Rcpp;

//[[Rcpp::export]]
DataFrame pcv_naivy(NumericMatrix x)
{
    // Vetor de inteiros para se referenciar a cada cidade
    IntegerVector int_cidades = seq_len(x.ncol()) - 1;
    CharacterVector char_cidades = as<CharacterVector>(int_cidades);

    // Garante que a primeira cidade é a origem
    IntegerVector rotas = seq_len(x.ncol()) - 1;
    rotas.erase(0);

    NumericVector custo;
    CharacterVector caminho;

    do
    {
        IntegerVector index = rotas;
        index.push_front(0);
        index.push_back(0);

        String it_caminho("0");
        double it_custo = 0;

        // cidades dentro do DO é a permutação da vez
        for (int i = 0; i < (index.size() - 1); i++)
        {
            double row = index[i];
            double col = index[i + 1];

            it_caminho += "-" + char_cidades[col];
            it_custo = it_custo + x(row, col);
        }

        caminho.push_back(it_caminho);
        custo.push_back(it_custo);

    } while (std::next_permutation(rotas.begin(), rotas.end()));

    // Seleciona os caminho com menor custo
    double menor_custo = min(custo);
    LogicalVector menor_rota_logical = (custo == menor_custo);
    IntegerVector menor_rota = seq_len(custo.length()) - 1;
    // Vetor com as possíveis cidades com menor custo
    menor_rota = menor_rota[menor_rota_logical];

    DataFrame df = DataFrame::create(
        Named("caminho") = caminho[menor_rota], 
        Named("custo") = custo[menor_rota]);
        
    return df;
}

