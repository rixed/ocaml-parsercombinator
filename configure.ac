AC_INIT(parsercombinator,0.1)
m4_include([m4/ocaml.m4])

AC_PROG_CC

AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
  AC_MSG_ERROR([You must install the OCaml compiler])
fi

AC_PROG_FINDLIB
if test "$OCAMLFIND" = "no"; then
  AC_MSG_ERROR([You must install OCaml findlib (the ocamlfind command)])
fi

AC_CHECK_OCAML_PKG(batteries portia lwt)

AC_CONFIG_FILES([Makefile make.common])
AC_OUTPUT
