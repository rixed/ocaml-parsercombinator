{ stdenv, fetchFromGitHub, ocaml, findlib, num, batteries, lwt_ppx, portia }:

stdenv.mkDerivation rec {
  pname = "ocaml${ocaml.version}-parsercombinator";
  version = "0.7";

  src = fetchFromGitHub {
    owner = "rixed";
    repo = "ocaml-parsercombinator";
    rev = "v${version}";
    sha256 = "1h2djnkdxmfdl27q5k3055bss4d62h6yrr151z1v7kq90rz5xfcv";
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
