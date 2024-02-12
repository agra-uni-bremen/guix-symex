# General

* Add packages for additional symbolic execution engines
    * BAP: https://github.com/BinaryAnalysisPlatform/bap
    * radius: https://github.com/aemmitt-ns/radius
    * MAAT: https://github.com/trailofbits/maat

# Package TODOs

TODOs for individual existing packages.

## KLEE

* Figure out if `klee` needs to propagate the `klee-uclibc` input
* Enable more tests (e.g. unit tests), currently on system tests are run.

## angr

* Upgrade to most recent angr version
    * Requires new version of Guix's python-rich package
    * The python-rich upgrade requires a python-pygmentize upgrade
    * Touching this two packages will result in a lot of rebuilds
* Enable tests for py-cle (requires angr/binaries)
* Double check packages with no custom test rule
    * The default test target doesn't error if no tests are run
* Debug remaining failing test for python-angr
