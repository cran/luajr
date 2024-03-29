#' Run Lua code
#'
#' Runs the specified Lua code.
#'
#' @param code Lua code block to run.
#' @param filename If non-`NULL`, name of file to run.
#' @param L [Lua state][lua_open] in which to run the code. `NULL` (default)
#' uses the default Lua state for \pkg{luajr}.
#' @return Lua value(s) returned by the code block converted to R object(s).
#' Only a subset of all Lua types can be converted to R objects at present.
#' If multiple values are returned, these are packaged in a `list`.
#' @examples
#' twelve <- lua("return 3*4")
#' print(twelve)
#' @export
lua = function(code, filename = NULL, L = NULL)
{
    if (is.null(filename)) {
        ret = .Call(`_luajr_run_code`, code, L)
    } else {
        ret = .Call(`_luajr_run_file`, filename, L)
    }

    if (is.null(ret)) invisible() else ret
}
