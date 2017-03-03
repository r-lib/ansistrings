
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

/* Does this `endcode` close this `startcode`? */

int ansistrings_i_re_closes(const char *startcode, const char *endcode) {

  if (endcode[0] == '3' && endcode[1] == '9') {
    return startcode[0] == '3' &&
      '0' <= startcode[1] && startcode[1] <= '8';

  } else if (endcode[0] == '4' && endcode[1] == '9') {
    return startcode[0] == '4' &&
      '0' <= startcode[1] && startcode[1] <= '8';

  } else if (endcode[0] == '2' && endcode[1] == '2') {
    return (startcode[0] == '1' || startcode[0] == '2') &&
      startcode[1] == '\0';

  } else if (endcode[0] == '2' && endcode[1] == '3') {
    return startcode[0] == '3' && startcode[1] == '\0';

  } else if (endcode[0] == '2' && endcode[1] == '4') {
    return startcode[0] == '4' && startcode[1] == '\0';

  } else if (endcode[0] == '2' && endcode[1] == '7') {
    return startcode[0] == '7' && startcode[1] == '\0';

  } else if (endcode[0] == '2' && endcode[1] == '8') {
    return startcode[0] == '8' && startcode[1] == '\0';

  } else if (endcode[0] == '2' && endcode[1] == '9') {
    return startcode[0] == '9' && startcode[1] == '\0';

  } else if (endcode[0] == '0') {
    return 1;
  }

  /* Unknown end tag */
  return 0;
}

SEXP ansistrings_make_ansi_map1(SEXP str, SEXP re_match_start,
				SEXP re_match_end, SEXP re_match_match,
				SEXP re_start_start, SEXP re_start_match,
				SEXP re_end_match) {

  int *pre_start_start = INTEGER(re_start_start);
  SEXP names, result = PROTECT(allocVector(VECSXP, 2));
  SEXP shifts =
    PROTECT(ansistrings_make_shifts1(re_match_start, re_match_end));

  SEXP map, rownames, res_names;
  int num_tags = LENGTH(re_match_start);
  int *active = (int *) R_alloc(num_tags, sizeof(int));
  int *mapmap = (int *) R_alloc(num_tags, sizeof(int));
  int activeptr = 0;
  int i, p;

#define is_start_tag(i) (pre_start_start[(i)] > 0)
#define is_end_tag(i)   (pre_start_start[(i)] <= 0)
  int num_start_tags = 0;

  for (i = 0; i < num_tags; i++) num_start_tags += is_start_tag(i);

  map = PROTECT(allocVector(VECSXP, 4));
  SET_VECTOR_ELT(map, 0, allocVector(INTSXP, num_start_tags));
  SET_VECTOR_ELT(map, 1, allocVector(INTSXP, num_start_tags));
  SET_VECTOR_ELT(map, 2, allocVector(STRSXP, num_start_tags));
  SET_VECTOR_ELT(map, 3, allocVector(STRSXP, num_start_tags));
  names = PROTECT(allocVector(STRSXP, 4));
  SET_STRING_ELT(names, 0, mkChar("start"));
  SET_STRING_ELT(names, 1, mkChar("end"));
  SET_STRING_ELT(names, 2, mkChar("open"));
  SET_STRING_ELT(names, 3, mkChar("close"));
  rownames = PROTECT(allocVector(INTSXP, 2));
  INTEGER(rownames)[0] = NA_INTEGER;
  INTEGER(rownames)[1] = -num_start_tags;
  setAttrib(map, R_NamesSymbol, names);
  setAttrib(map, R_RowNamesSymbol, rownames);
  setAttrib(map, R_ClassSymbol, ScalarString(mkChar("data.frame")));
  UNPROTECT(2);  		/* names, rownames */

  PROTECT(res_names = allocVector(STRSXP, 2));
  SET_STRING_ELT(res_names, 0, mkChar("map"));
  SET_STRING_ELT(res_names, 1, mkChar("shifts"));
  setAttrib(result, R_NamesSymbol, res_names);
  UNPROTECT(1);			/* res_names */

  /* These will be modified in place, so we can put them in now */
  SET_VECTOR_ELT(result, 0, map);
  SET_VECTOR_ELT(result, 1, shifts);
  UNPROTECT(2);			/* map, shifts */

  /* Is there anything to do? */
  if (num_start_tags == 0) {
    UNPROTECT(1);		/* result */
    return result;
  }

  for (i = 0, p = 0 ; i < num_tags; i++) {
    if (is_start_tag(i)) {
      active[activeptr++] = i;
      mapmap[i] = p;
      INTEGER(VECTOR_ELT(map, 0))[p] = INTEGER(shifts)[i];
      INTEGER(VECTOR_ELT(map, 1))[p] = 0; /* instead of memset(0) */
      SET_STRING_ELT(VECTOR_ELT(map, 2), p, STRING_ELT(re_match_match, i));
      p++;

    } else {
      const char *endcode = CHAR(STRING_ELT(re_end_match, i));
      const char *endtag = CHAR(STRING_ELT(re_match_match, i));

      /* Need to go over the active tags, to see which one is closed by
	 this end tag. */
      int j;
      for (j = 0; j < activeptr; ) {
	int a = active[j];
	int m = mapmap[a];
	const char *startcode = CHAR(STRING_ELT(re_start_match, a));
	int closes = ansistrings_i_re_closes(startcode, endcode);
	if (closes) {
	  INTEGER(VECTOR_ELT(map, 1))[m] = INTEGER(shifts)[i] - 1;
	  SET_STRING_ELT(VECTOR_ELT(map, 3), m, mkChar(endtag));
	  active[j] = active[activeptr];
	  activeptr--;
	} else {
	  j++;
	}
      }
    }
  }

  /* Close at the end of the string, if the ANSI close tag is missing */
  p = strlen(CHAR(STRING_ELT(str, 0))) - INTEGER(shifts)[num_tags - 1];
  for (i = 0; i < num_start_tags; i++) {
    if (INTEGER(VECTOR_ELT(map, 1))[i] == 0) {
      INTEGER(VECTOR_ELT(map, 1))[i] = p;
      SET_STRING_ELT(VECTOR_ELT(map, 3), i, mkChar(""));
    }
  }

  UNPROTECT(1);			/* result */
  return result;
}

#undef is_start_tag
#undef is_end_tag
