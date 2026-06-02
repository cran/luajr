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
luaprint = lua_func("print")
luaprint("Hello, world")

## -----------------------------------------------------------------------------
lua("function excited_print(x) print(x .. '!') end")
lua("return excited_print('Hello, world')")

xp = lua_func("excited_print", "native")
xp("Wow")

## -----------------------------------------------------------------------------
timestwo = lua_func("function(x) return x*2 end", "native")
timestwo(123)

## -----------------------------------------------------------------------------
values = c(1.0, 2.0, 3.0)
keep = lua_func("function(x) x[1] = 999 end", ".") # passed by value
keep(values)
print(values)

change = lua_func("function(x) x[1] = 999 end", "&.") # passed by reference
change(values)
print(values)

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

