# Change Log

## 0.18.0

The Eliom part of Subsocia has been put into a separate package.  For change
logs related to earlier versions of this code, see the Subsocia package.

The main motivation was to change the build system of the core package to
dune aka jbuilder while keeping packaging simple.  The split also involves
creating separate findlib packages, which avoid additional dependencies in
case the web interface is not needed.
