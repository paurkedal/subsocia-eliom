(lang dune 3.19)
(name subsocia-eliom)

(dialect
 (name "eliom-server")
 (implementation (extension "eliom"))
 (interface (extension "eliomi")))

(dialect
 (name "eliom-server-shared")
 (implementation (extension "shared.ml"))
 (interface (extension "shared.mli")))

(dialect
 (name "eliom-server-server")
 (implementation (extension "server.ml"))
 (interface (extension "server.mli")))

(wrapped_executables false)
