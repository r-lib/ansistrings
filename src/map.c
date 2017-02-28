
#include <R.h>
#include <Rinternals.h>

SEXP ansistrings_make_shifts1(SEXP start, SEXP end) {
  size_t i, n = LENGTH(start);
  SEXP result = PROTECT(allocVector(INTSXP, n * 3));
  SEXP dim = PROTECT(allocVector(INTSXP, 2));

  INTEGER(dim)[0] = n;
  INTEGER(dim)[1] = 3;
  setAttrib(result, R_DimSymbol, dim);
  
  for (i = 0; i < n; i++) {
    INTEGER(result)[n + i] = INTEGER(start)[i];
    INTEGER(result)[2 * n + i] = INTEGER(end)[i] - INTEGER(start)[i] + 1;
  }

  for (i = 1; i < n; i++) {
    INTEGER(result)[2 * n + i] += INTEGER(result)[2 * n + i - 1];
  }

  if (n > 0) INTEGER(result)[0] = INTEGER(result)[n];
  for (i = 1; i < n; i++) {
    INTEGER(result)[i] =
      INTEGER(result)[n + i] - INTEGER(result)[2 * n + i - 1];
  }
  
  UNPROTECT(2);
  return result;
}

SEXP ansistrings_map_raw_to_ansi1(SEXP shifts, SEXP raw) {
  SEXP dim = getAttrib(shifts, R_DimSymbol);
  size_t i = 0, n = INTEGER(dim)[0];
  int craw = INTEGER(raw)[0];

  while (i < n && craw >= INTEGER(shifts)[i]) i++;

  return ScalarInteger(i == 0 ? craw : craw + INTEGER(shifts)[2 * n + i - 1]);
}

SEXP ansistrings_map_ansi_to_raw1(SEXP shifts, SEXP ansi) {
  SEXP dim = getAttrib(shifts, R_DimSymbol);
  size_t i = 0, n = INTEGER(dim)[0];
  int cansi = INTEGER(ansi)[0];

  while (i < n && cansi >= INTEGER(shifts)[n + i]) i++;

  return ScalarInteger(i == 0 ? cansi : cansi - INTEGER(shifts)[2 * n + i - 1]);
}
