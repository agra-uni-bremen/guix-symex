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
* [BINSEC]: Binary-level symbolic execution platform.
* [SymEx-VP]: Symbolic execution of [RISC-V] binaries with support for [SystemC] hardware models.

More packages will be added in the future.
Furthermore, patches adding additional additional symbolic execution engines are more than welcome.
Nonetheless, in the long run, the goal is to integrate all packages into upstream Guix at some point (see below).

## Setup

Naturally, this channel requires Guix to be installed.
If Guix is not yet installed no your system, refer to the [Guix installation instructions][guix install].
Also note that many [Linux distributions][guix repology] (such as Alpine, Debian, or Ubuntu) provide packages for Guix which may ease installation.
One Guix is successfully installed and configured on your system, this channel can be enabled by adding it to `~/.config/guix/channels.scm`:

```scheme
(cons* (channel
        (name 'symex)
        (url "https://github.com/agra-uni-bremen/guix-symex")
        (introduction
          (make-channel-introduction
            "6dc013a390d3abea0faa32246fc4399085d1ba3a"
            (openpgp-fingerprint
              "514E 833A 8861 1207 4F98  F68A E447 3B6A 9C05 755D"))))
       %default-channels)
```

For more information on channels, refer to the [Guix manual][guix channel].

## Usage

After configuring the channel successfully run `guix pull` to fetch the latest version of this new channel.
If this command succeeds, you should be able to use package provided by this channel.
As an example, run:

    $ guix shell klee

This command will drop you into an interactive shell where the KLEE symbolic execution engine (as provided by this channel) is available.
Refer to [invoking `guix shell`][guix-shell] for more information on this interactive environment.

For artifact evaluation purposes, you can generate a Docker or VM Image which has selected symbolic execution engines installed in binary form.
As an example, the following command will generate a Docker image where both KLEE and BINSEC are installed:

    $ guix pack -f docker \
        --image-tag="symex-image" \
        -S /bin=bin \
        -S /usr/bin=bin \
        klee binsec

This will take a while and, if successful, print a file path along the lines of `/gnu/store/…-klee-docker-pack.tar.gz`.
This file represents the generated Docker image.
In order to use this image with Docker run the following commands:

    $ docker load < /gnu/store/…-klee-docker-pack.tar.gz
    $ docker run -it symex-image

For more information in this regard refer to the documentation of [`guix pack`][guix-pack] and [`guix-system`][guix-system] in the Guix manual.
Additionally, the paper [*Toward practical transparent verifiable and long-term reproducible research using Guix*][long-term reproduce] also provides a nice introduction regarding long-term reproducible research with Guix.

## Upstream Status

The following packages are currently in the process of being upstreamed:

* KLEE: https://issues.guix.gnu.org/68296
* BINSEC: https://issues.guix.gnu.org/68908
* angr: https://issues.guix.gnu.org/69074
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
[BINSEC]: https://github.com/binsec/binsec
[guix install]: https://guix.gnu.org/en/manual/devel/en/html_node/Installation.html
[guix repology]: https://repology.org/project/guix
[guix-shell]: https://guix.gnu.org/en/manual/devel/en/html_node/Invoking-guix-shell.html
[guix-pack]: https://guix.gnu.org/en/manual/devel/en/html_node/Invoking-guix-pack.html
[guix-system]: https://guix.gnu.org/manual/en/html_node/Invoking-guix-system.html#index-virtual-machine
