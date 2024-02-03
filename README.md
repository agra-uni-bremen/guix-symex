# guix-symex

A [Guix] [channel][guix channel] containing packages for several [symbolic execution] engines.

## Motivation

This is a channel for the functional package manager Guix which provides packages for several symbolic execution engines.
The goal of this channel is to ease building long-term reproducible computational environments along the lines outlined in [prior work][long-term reproduce].
Thereby, easing empirical comparisons with prior work in the symbolic execution domain.
Additionally, the channel should also enable practitioners to install and setup various symbolic execution engines for software testing and software analysis purposes.

Currently, the following symbolic execution engines are packaged:

* [KLEE]: A symbolic execution engine for which is based on the LLVM compiler infrastructure.
* [angr]: A Python framework for symbolic execution of software in binary form.
* [BinSec]: Binary-level symbolic execution platform.
* [SymEx-VP]: Symbolic execution of [RISC-V] binaries with support for [SystemC] hardware models.

Packages for additional engines are more than welcome.
Nonetheless, in the long run, the goal is to integrate all packages into upstream Guix at some point (see below).

## Usage

Naturally, this channel requires Guix to be installed.
If Guix is not yet installed no your system, refer to the [Guix installation instructions][guix install].
Also note that many [Linux distributions][guix repology] (such as Alpine, Debian, or Ubuntu) provide packages for Guix which may ease installation.
One Guix is successfully installed and configured on your system, this channel can be enabled by adding it to `~/.config/guix/channels.scm`:

```scheme
(cons* (channel
        (name 'symex)
        (url "https://github.com/nmeum/guix-symex")
       %default-channels)
```

For more information on channels, refer to the [Guix manual][guix channel].

## Upstream Status

The following packages are currently in the process of being upstreamed:

* KLEE: https://issues.guix.gnu.org/68296
* …

More packages will be proposed for upstream integration soon.

## License

This program is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
Public License for more details.

You should have received a copy of the GNU General Public License along
with this program. If not, see <https://www.gnu.org/licenses/>.

[Guix]: https://guix.gnu.org
[guix channel]: https://guix.gnu.org/en/manual/devel/en/html_node/Channels.html
[symbolic execution]: https://en.wikipedia.org/wiki/Symbolic_execution
[long-term reproduce]: https://doi.org/10.1038/s41597-022-01720-9
[LLVM]: https://llvm.org/
[KLEE]: https://klee.github.io/
[SymEx-VP]: https://github.com/agra-uni-bremen/symex-vp
[RISC-V]: https://riscv.org/
[SystemC]: https://systemc.org
[angr]: https://angr.io
[BinSec]: https://github.com/binsec/binsec
[guix install]: https://guix.gnu.org/en/manual/devel/en/html_node/Installation.html
[guix repology]: https://repology.org/project/guix
