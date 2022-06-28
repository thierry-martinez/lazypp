(lang dune 2.8)
(generate_opam_files true)

(using menhir 2.0)

(name lazypp)
(license "BSD-2-Clause")
(maintainers "Thierry Martinez <thierry.martinez@inria.fr>")
(authors "Thierry Martinez <thierry.martinez@inria.fr>")
(source (uri "git+https://github.com/thierry.martinez/lazypp.git"))
(homepage "https://github.com/thierry.martinez/lazypp")
(bug_reports "https://github.com/thierry.martinez/lazypp")
(documentation "https://github.com/thierry.martinez/lazypp")
(version "1.0.1")

(package
 (name lazypp)
 (synopsis "Lazy C/C++ preprocessor")
 (description "\
lazypp preprocesses partially C/C++ source code
")
 (depends
  (ocaml (and (>= 4.03.0) (< 4.12.0)))
  (dune (>= 1.11.3))
  (cmdliner (>= 1.0.4))
  (redirect (>= 0.1.1))
  (stdcompat (>= 10))
  (odoc (and :with-doc (>= 1.5.1)))))
