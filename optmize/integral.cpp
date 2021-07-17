// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(RcppNumerical)]]

#include <RcppNumerical.h>

using namespace Numer;

class myfun : public Func
{
private:
    const double beta0;
    const double sigma2;
    double y;

public:
    myfun(const double beta0_, const double sigma2_, double y_) : beta0(beta0_), sigma2(sigma2_), y(y_) {}

    double operator()(const double &x) const
    {
        return R::dpois(y, std::exp(beta0 + x), 0) * R::dnorm(x, 0, std::sqrt(sigma2), 0);
    }
};

// [[Rcpp::export]]
Rcpp::NumericVector integrate_poisson(
    Rcpp::NumericVector y,
    const double beta0,
    const double sigma2)
{
    const double lower = R_NegInf, upper = R_PosInf;
    double err_est;
    int err_code;

    for (int i = 0; i <= y.length(); i++)
    {
        myfun f(beta0, sigma2, y[i]);
        const double res = integrate(f, lower, upper, err_est, err_code);
        y[i] = res;
    }
    return (y);
}

// [[Rcpp::export]]
double log_integrate_poisson(
    Rcpp::NumericVector par,
    Rcpp::NumericVector y)
{
    const double beta0 = par[0];
    const double sigma2 = std::exp(par[1]);
    Rcpp::NumericVector ll = integrate_poisson(y, beta0, sigma2);
    ll = Rcpp::log(ll);
    double res = Rcpp::sum(ll);

    return res * -1;
}

// // [[Rcpp::export]]
// double log_integrate_poisson_par(
//     Rcpp::NumericVector par,
//     Rcpp::NumericVector y)
// {
//     const double beta0 = par[0];
//     const double sigma2 = std::exp(par[1]);
//     Rcpp::NumericVector ll = integrate_poisson(y, beta0, sigma2);
//     ll = Rcpp::log(ll);
//     double res = Rcpp::sum(ll);

//     return res * -1;
// }