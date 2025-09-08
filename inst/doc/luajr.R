## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(luajr)

## -----------------------------------------------------------------------------
lua("return 'Hello ' .. 'world!'")

## -----------------------------------------------------------------------------
lua("my_animal = 'walrus'")
lua("return my_animal")

## -----------------------------------------------------------------------------
lua("local my_animal = 'donkey'")
lua("return my_animal")

## -----------------------------------------------------------------------------
lua("local my_veg = 'potato'; local my_dish = my_veg .. ' pie'; return my_dish")

## -----------------------------------------------------------------------------
luatype = lua_func("type")
luatype(TRUE)

## -----------------------------------------------------------------------------
lua("function squared(x) return x^2 end")
lua("return squared(4)")

sq = lua_func("squared")
sq(8)

## -----------------------------------------------------------------------------
timestwo = lua_func("function(x) return x*2 end")
timestwo(123)

## -----------------------------------------------------------------------------
values = c(1.0, 2.0, 3.0)
keep = lua_func("function(x) x[1] = 999 end", "v") # passed by value
keep(values)
print(values)

change = lua_func("function(x) x[1] = 999 end", "r") # passed by reference
change(values)
print(values)

## -----------------------------------------------------------------------------
x = list(1)

f1 = lua_func("function(x) x[1][1] = 999; x.a = 42; end", "v")
f1(x)
print(x)

f2 = lua_func("function(x) x[1][1] = 999; x.a = 42; end", "r")
f2(x)
print(x)

## -----------------------------------------------------------------------------
x = list(1)

f3 = lua_func("function(x) x[1][1] = 999; x.a = 42; return x; end", "v")
x = f3(x)
print(x)

f4 = lua_func("function(x) x[1] = luajr.numeric({888, 999}); return x; end", "v")
x = f4(x)
print(x)

## -----------------------------------------------------------------------------
v1 = rnorm(1e1)
v4 = rnorm(1e4)
v7 = rnorm(1e7)

lua("sum2 = function(x) local s = 0; for i=1,#x do s = s + x[i]*x[i] end; return s end")
sum2 = function(x) sum(x*x)
sum2_r = lua_func("sum2", "r")
sum2_v = lua_func("sum2", "v")
sum2_s = lua_func("sum2", "s")

# Comparing the results of each function:
sum2(v1)    # Pure R version
sum2_r(v1)  # luajr pass-by-reference
sum2_v(v1)  # luajr pass-by-value
sum2_s(v1)  # luajr pass-by-simplify

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
    local result = luajr.dataframe()
    result.a = luajr.numeric_r(dflen, 0)
    result.x = luajr.numeric_r(dflen, 0)
    
    local j = 1
    for k,a in pairs(A) do
        local x = x0
        for i = 1, burn do
            x = a * x * (1 - x)
        end
        for i = 1, iter do
            result.a[j] = a
            result.x[j] = x
            x = a * x * (1 - x)
            j = j + 1
        end
    end
    
    return result
end", "sssr")

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
L1 = lua_open()
lua("a = 2")
lua("a = 4", L = L1)
lua("return a")
lua("return a", L = L1)

## -----------------------------------------------------------------------------
lua("a = 2")
lua("return a")
lua_reset()
lua("return a")
#> NULL

