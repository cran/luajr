#' Make a Lua function callable from R
#'
#' Takes any Lua expression that evaluates to a function and provides an R
#' function that can be called to invoke the Lua function.
#'
#' The R types that can be passed to Lua are: `NULL`, logical vector,
#' integer vector, numeric vector, string vector, list, external pointer, and
#' raw.
#'
#' The parameter `argcode` is a string with one character for each argument of
#' the Lua function, recycled as needed (e.g. so that a single character would
#' apply to all arguments regardless of how many there are).
#'
#' In the following, the corresponding character of `argcode` for a specific
#' argument is referred to as its argcode.
#'
#' For `NULL` or any argument with length 0, the result in Lua is **nil**
#' regardless of the corresponding argcode.
#'
#' For logical, integer, double, and character vectors, if the corresponding
#' argcode is `'s'` (simplify), then if the R vector has length one, it is
#' supplied as a Lua primitive (boolean, number, number, or string,
#' respectively), and if length > 1, as an array, i.e. a table with integer
#' indices starting at 1. If the code is `'a'`, the vector is always supplied as
#' an array, even if it only has length 1. If the argcode is the digit `'1'`
#' through `'9'`, this is the same as `'s'`, but the vector is required to have
#' that specific length, otherwise an error message is emitted.
#'
#' Still focusing on the same vector types, if the argcode is `'r'`, then the
#' vector is passed *by reference* to Lua, adopting the type `luajr.logical_r`,
#' `luajr.integer_r`, `luajr.numeric_r`, or `luajr.character_r` as appropriate.
#' If the argcode is `'v'`, the vector is passed *by value* to Lua,
#' adopting the type `luajr.logical`, `luajr.integer`, `luajr.numeric`, or
#' `luajr.character` as appropriate.
#'
#' For a raw vector, only the `'s'` type is accepted and the result in Lua is
#' a string (potentially with embedded nulls).
#'
#' For lists, if the argcode is `'s'` (simplify), the list is passed as a Lua
#' table. Any entries of the list with non-blank names are named in the table,
#' while unnamed entries have the associated integer key in the table. Note that
#' Lua does not preserve the order of entries in tables. This means that an R
#' list with names will often go "out of order" when passed into Lua with `'s'`
#' and then returned back to R. This is avoided with argcode `'r'` or `'v'`.
#'
#' If a list is passed in with the argcode `'r'` or `'v'`, the list is
#' passed to Lua as type `luajr.list`, and all vector elements of the list are
#' passed by reference or by value, respectively.
#'
#' For external pointers, the argcode is ignored and the external pointer is
#' passed to Lua as type **userdata**.
#'
#' When the function is called and Lua values are returned from the function,
#' the Lua return values are converted to R values as follows.
#'
#' If nothing is returned, the function returns `invisible()` (i.e. `NULL`).
#'
#' If multiple arguments are returned, a list with all arguments is returned.
#'
#' Reference types (e.g. `luajr.logical_r`) and vector types (e.g.
#' `luajr.logical`) are returned to R as such. A `luajr.list` is returned as an
#' R list. Reference and list types respect R attributes set within Lua code.
#'
#' A **table** is returned as a list. In the list, any table entries with a
#' number key come first (with indices 1 to n, i.e. the original number key's
#' value is discarded), followed by any table entries with a string key
#' (named accordingly). This may well scramble the order of keys, so beware.
#' Note in particular that Lua does not guarantee that it will traverse a table
#' in ascending order of keys. Entries with non-number, non-string keys are
#' discarded. It is probably best to avoid returning a **table** with anything
#' other than string keys, or to use `luajr.list`.
#'
#' A Lua string with embedded nulls is returned as an R raw type.
#'
#' @inheritParams lua
#' @param func Lua expression evaluating to a function.
#' @param argcode How to wrap R arguments for the Lua function.
#' @return An R function which can be called to invoke the Lua function.
#' @examples
#' squared <- lua_func("function(x) return x^2 end")
#' print(squared(7))
#' @export
lua_func = function(func, argcode = "s", L = NULL)
{
    fx = .Call(`_luajr_func_create`, func, L);
    return (function(...) {
        # Call the function.
        ret = .Call(`_luajr_func_call`, fx, list(...), argcode, L);

        # If ret is NULL, return it invisibly.
        if (is.null(ret)) {
            return (invisible())
        } else {
            return (ret)
        }
    })
}

