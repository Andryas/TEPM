#include <Rcpp.h>

// [[depends(RcppEigen)]]
// [[depends(RcppNumerical)]]

using namespace Rcpp;

//[[Rcpp::export]]
DataFrame pcv_nearest_neighbor (NumericMatrix x) 
{

    NumericMatrix m(clone(x));

    // Vetor de inteiros para se referenciar a cada cidade
    IntegerVector int_cidade = seq_len(x.ncol()) - 1;
    // Vetor de inteiros para se referenciar as cidades não visitadas
    IntegerVector unvisited = seq_len(x.ncol()) - 1;
    // A cidade[0] é a origem
    unvisited.erase(0);
    // Sequência de cidades percorridas
    IntegerVector caminho;
    caminho.push_back(0);
    // Custo (Distância) percorrida
    double custo;

    NumericVector teste;

    while (true)
    {
        // Última cidade destino
        int origem = caminho[(caminho.length() - 1)];
        // Distâncias da origem para os destinos
        NumericVector destino = m(origem, _ ) ;
        // Vetor somente com as possíveis cidades (não visitadas)
        IntegerVector inf_cidades = setdiff(int_cidade, unvisited);

        // Para as cidades visitadas troca a distancia por infinito
        for (int i = 0; i < inf_cidades.length(); ++i)
        {
            // Rprintf("the value of inf_cidades[%i] : %f \n", i, inf_cidades[i]);
            destino[inf_cidades[i]] = R_PosInf;
        }

        // Menor distância
        double menor_distancia = min(destino);
        LogicalVector menor_distancia_logical = (destino == menor_distancia);
        IntegerVector cidade_destino = seq_len(destino.length())-1;
        // Vetor com as possíveis cidades com menor custo
        cidade_destino = cidade_destino[menor_distancia_logical];
        // Seleciona sempre a primeira cidade que aparecer com menor custo
        double custo_destino = m(origem, cidade_destino[0]);

        caminho.push_back(cidade_destino[0]);
        custo = custo + custo_destino;

        LogicalVector logical_unvisited = !(unvisited == cidade_destino[0]);
        unvisited = unvisited[logical_unvisited];

        if (unvisited.length() == 0)
        {
            break;
        }
    }

    // Liga o último destino com a origem
    int origem = caminho[(caminho.length() - 1)];
    double custo_destino = m(origem, 0);
    caminho.push_back(0);
    custo = custo + custo_destino;

    CharacterVector char_caminho = as<CharacterVector>(caminho);
    String concatena_caminho;

    // Concatena o caminho percorrido
    for (int i = 0; i < (char_caminho.size()); i++)
    {
        concatena_caminho += "-" + char_caminho[i];
    }
    concatena_caminho.replace_first("-", "");

    DataFrame df = DataFrame::create(
        Named("caminho") = concatena_caminho,
        Named("custo") = custo);

    return df;
}
