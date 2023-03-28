# Change Log

## 0.19.0

  - Revise authentication and configuration, adding support for JWT and
    trusted environment variables.
  - Don't restrict attribute source entities to covers.
  - Use `Iso639` directly, instead of `Panograph_i18n`.
  - Collate display names in entity list using Confero.
  - Switch to the LGPL-3.0 Linking Exception.

## 0.18.2

  - Support js\_of\_ocaml 3.7.
  - Style adjustments.

## 0.18.1

- Use `Logs` for inclusion change log, and it possible to configure a target
  log file.
- Print out both ID and display name for author and entities in inclusion
  change log.

## 0.18.0

The Eliom part of Subsocia has been put into a separate package.  For change
logs related to earlier versions of this code, see the Subsocia package.

The main motivation was to change the build system of the core package to
dune aka jbuilder while keeping packaging simple.  The split also involves
creating separate findlib packages, which avoid additional dependencies in
case the web interface is not needed.
