#ifndef LUAJR_API_H
#define LUAJR_API_H

struct SEXPREC;
typedef struct SEXPREC* SEXP;

// Lua and LuaJIT APIs
#include "luajr_lua.h"
#include "luajr_lauxlib.h"
#include "luajr_lualib.h"
#include "luajr_luajit.h"

// luajr API functions
// TODO this is easy to break if these do not match API funcs defined elsewhere.
extern SEXP (*luajr_open)();
extern SEXP (*luajr_reset)();
extern lua_State* (*luajr_newstate)();
extern lua_State* (*luajr_getstate)(SEXP Lx);
extern void (*luajr_pushsexp)(lua_State* L, SEXP x, char as);
extern SEXP (*luajr_tosexp)(lua_State* L, int index);
extern void (*luajr_pass)(lua_State* L, SEXP args, const char* acode);
extern SEXP (*luajr_return)(lua_State* L, int nret);
extern SEXP (*luajr_run_code)(SEXP code, SEXP Lx);
extern SEXP (*luajr_run_file)(SEXP filename, SEXP Lx);
extern SEXP (*luajr_func_create)(SEXP code, SEXP Lx);
extern SEXP (*luajr_func_call)(SEXP fx, SEXP alist, SEXP acode, SEXP Lx);
extern void (*luajr_pushfunc)(SEXP fx);
extern SEXP (*luajr_run_parallel)(SEXP func, SEXP n, SEXP threads, SEXP pre);
extern SEXP (*luajr_makepointer)(void* ptr, int tag_code, void (*finalize)(SEXP));
extern void* (*luajr_getpointer)(SEXP x, int tag_code);
extern void (*luajr_pcall)(lua_State* L, int nargs, int nresults, const char* funcdesc);

#endif // LUAJR_API_H
