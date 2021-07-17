## Projeto: Modelo Poisson com efeito aleatório --------------------------------
## Prof. Wagner Hugo Bonat LEG/UFPR --------------------------------------------
## Data: 18/06/2021 ------------------------------------------------------------

profvis::profvis({
  ## Fixando valores do parâmetros
  sigma2 <- 0.5
  beta0 <- log(10)

  # Tamanho da amostra
  set.seed(123)
  n = 100

  ## Simular o efeito aleatório b_i
  bi <- rnorm(n = n, mean = 0, sd = sqrt(sigma2))
  bi

  ## Preditor linear
  lambdai <- exp(beta0 + bi)

  ## Simulando os dados Yi's
  yi <- rpois(n = n, lambda = lambdai)
  hist(yi)

  ## FIM DA PARTE DE SIMULAÇÃO ---------------------------------------------------

  integrando <- function(bi, beta0, sigma2, y) {
    lambda <- exp(beta0 + bi)
    out <- dpois(y, lambda = lambda)*dnorm(bi, mean = 0, sd = sqrt(sigma2))
    return(out)
  }

  ## Gráfico do integrando
  b <- seq(-3, 3, l = 100)
  plot(integrando(bi = b, beta0 = log(10), sigma2 = 0.5, y = yi[1]) ~ b, type = "l")
  for(i in 2:100) {
    lines(b, integrando(bi = b, beta0 = log(10), sigma2 = 0.5, y = yi[i]))
  }

  ## Resolvendo a integral
  integral <- c()
  for(i in 1:100) {
    integral[i] <- integrate(f = integrando, lower = -Inf, upper = Inf, 
                            beta0 = beta0, sigma2 = sigma2, y = yi[i])$value
  }

  ll <- sum(log(integral))
  ll

  ## Criando uma função: log-verossimilhança
  my_ll <- function(par, y) {
    integral <- c()
    for(i in 1:length(y)) {
      integral[i] <- integrate(f = integrando, lower = -Inf, upper = Inf, 
                              beta0 = par[1], sigma2 = exp(par[2]), y = y[i])$value
    }
    print(c(round(par, 2)))
    ll <- sum(log(integral))
    return(-ll)
  }

  my_ll(par = c(beta0, log(sigma2) ), y = yi)

  invisible(temp <- optim(
    par = c(log(8), log(0.3)), fn = my_ll, y = yi,
    method = "Nelder-Mead"
  ))
  temp
  invisible(temp2 <- optim(
    par = c(log(8), log(0.3)), fn = my_ll, y = yi,
    method = "BFGS"
  ))
  temp2
})
str(p)

p[[1]]
