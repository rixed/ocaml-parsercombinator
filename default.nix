{ stdenv, fetchFromGitHub, ocaml, findlib, num, batteries, lwt_ppx, portia }:

stdenv.mkDerivation rec {
  pname = "ocaml${ocaml.version}-parsercombinator";
  version = "0.8";

  src = fetchFromGitHub {
    owner = "rixed";
    repo = "ocaml-parsercombinator";
    rev = "v${version}";
    sha256 = "0154zbp29ndkjv4x1xlaxvdmc73vf1253z59hxab2j1h40bsci4q";
  };

  buildInputs = [ ocaml findlib num batteries lwt_ppx portia ];

  createFindlibDestdir = true;

  meta = with stdenv.lib; {
    homepage = https://github.com/rixed/ocaml-parsercombinator;
    description = "Resumable combinatoric parser with error detection";
    platforms = ocaml.meta.platforms or [];
    maintainers = [ maintainers.rixed ];
  };
}
