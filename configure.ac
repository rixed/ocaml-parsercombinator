AC_INIT(parsercombinator, 1.1.0)
m4_include([m4/ocaml.m4])

AC_PROG_CC

AC_PROG_OCAML
AS_IF([test "$OCAMLC" = "no"], [
  AC_MSG_ERROR([You must install the OCaml compiler])
])

AC_PROG_FINDLIB
if test "$OCAMLFIND" = "no"; then
  AC_MSG_ERROR([You must install OCaml findlib (the ocamlfind command)])
fi

AC_CHECK_OCAML_PKG([batteries])
AS_IF([test "$OCAML_PKG_batteries" = "no"], [
  AC_MSG_ERROR([OCaml package batteries not found])
])
AC_CHECK_OCAML_PKG([portia])
AS_IF([test "$OCAML_PKG_portia" = "no"], [
  AC_MSG_ERROR([OCaml package portia not found])
])
AC_CHECK_OCAML_PKG([lwt_ppx])
AS_IF([test "$OCAML_PKG_lwt_ppx" = "no"], [
  AC_MSG_ERROR([OCaml package lwt_ppx not found])
])

AC_CONFIG_FILES([Makefile make.common opam])
AC_OUTPUT
