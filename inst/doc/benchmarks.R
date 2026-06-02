## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(luajr)

x <- rnorm(1e6)

# Method one: built-in sum
s1 <- sum(x)

# Method two: sum in R
sum_R <- function(x) {
    s <- 0
    for (y in x) {
        s <- s + y
    }
    return (s)
}
s2 <- sum_R(x)

# Method three: sum in Lua
sum_L <- lua_func("function(x)
    local s = 0
    for i = 1, #x do
        s = s + x[i]
    end
    return s
end")
s3 <- sum_L(x)

## -----------------------------------------------------------------------------
logistic_map_R = function(x0, burn, iter, A)
{
    result_x = numeric(length(A) * iter)
    
    j = 1
    for (a in A) {
        x = x0
        for (i in 1:burn) { 
            x = a * x * (1 - x)
        }
        for (i in 1:iter) { 
            result_x[j] = x
            x = a * x * (1 - x)
            j = j + 1
        }
    }
    
    return (list2DF(list(a = rep(A, each = iter), x = result_x)))
}

logistic_map_L = lua_func(
"function(x0, burn, iter, A)
    local dflen = #A * iter
    local aa = luajr.numeric(dflen, 0)
    local xx = luajr.numeric(dflen, 0)
    
    local j = 1
    for k, a in pairs(A) do
        local x = x0
        for i = 1, burn do
            x = a * x * (1 - x)
        end
        for i = 1, iter do
            aa[j] = a
            xx[j] = x
            x = a * x * (1 - x)
            j = j + 1
        end
    end
    
    local result = luajr.dataframe()
    result:set('a', aa)
    result:set('x', xx)
    
    return result
end", "native, native, native, auto")

# To be compiled using Rcpp::cppFunction()
logistic_map_C =
'DataFrame logistic_map(double x0, unsigned int burn, unsigned int iter, NumericVector A)
{
    unsigned int dflen = A.length() * iter;
    NumericVector da(dflen, 0);
    NumericVector dx(dflen, 0);
    
    unsigned int j = 0;
    for (auto a : A)
    {
        double x = x0;
        for (unsigned int i = 0; i < burn; ++i)
            x = a * x * (1 - x);
        for (unsigned int i = 0; i < iter; ++i, ++j)
        {
            dx[j] = x;
            da[j] = a;
            x = a * x * (1 - x);
        }
    }

    return DataFrame::create(Named("a") = da, Named("x") = dx);
}'

## -----------------------------------------------------------------------------
logistic_map = logistic_map_L(0.5, 100, 100, 200:385/100)
plot(logistic_map$a, logistic_map$x, pch = ".")

## -----------------------------------------------------------------------------
# Lorenz attractor in R

# Take a single step, size dt, of the Lorenz system
step1 = function(x, rho, sigma, beta, dt)
{
    dx = sigma * (x[2] - x[1])
    dy = x[1] * (rho - x[3]) - x[2]
    dz = x[1] * x[2] - beta * x[3]
    x[1] = x[1] + dx * dt
    x[2] = x[2] + dy * dt
    x[3] = x[3] + dz * dt
    return (x)
}

# Calculate Lorenz system trajectory
lorenz1 = function(n, init, rho, sigma, beta)
{
    x = init
    result = matrix(0, nrow = n, ncol = 3)
    for (i in 1:n) {
        result[i, ] = x
        x = step1(x, rho, sigma, beta, 0.01)
    }
    return (result)
}

# Generate and plot result
result1 = lorenz1(5000, c(1,1,1), 28, 10, 8/3)
plot(result1[,1], result1[,2], type = "l")

## -----------------------------------------------------------------------------
# Lorenz attractor in Lua
library(luajr)

lorenz2 = lua_func("
function(n, init, rho, sigma, beta)
    local step2 = function(x, rho, sigma, beta, dt)
        local dx = sigma * (x[2] - x[1])
        local dy = x[1] * (rho - x[3]) - x[2]
        local dz = x[1] * x[2] - beta * x[3]
        x[1] = x[1] + dx * dt
        x[2] = x[2] + dy * dt
        x[3] = x[3] + dz * dt
        return x
    end

    local x = init
    local result = luajr.matrix(n, 3)

    for i = 1,n do
        result[i] = x[1]
        result[i + n] = x[2]
        result[i + 2*n] = x[3]
        x = step2(x, rho, sigma, beta, 0.01)
    end

    return result
end", "native auto native")

result2 = lorenz2(5000, c(1,1,1), 28, 10, 8/3)
plot(result2[,1], result2[,2], type = "l")

