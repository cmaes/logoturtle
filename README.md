# A Logo Interperter in Ocaml (and Javascript)

## Try  the interperter in your browser

Visit http://cmaes.github.io/logoturtle/


## Installation

The following directions describe how to install the dependencies for
running the code locally.

### Install OCAML

Use the Real World OCAML installation instructions
https://github.com/realworldocaml/book/wiki/Installation-Instructions

#### On Linux

```
$ sudo add-apt-repository ppa:avsm/ppa
$ sudo apt-get update
$ sudo apt-get install curl build-essential m4 ocaml opam
```

Then do
```
opam init
eval `opam config env`
```

#### On Mac

Install Homebrew http://brew.sh.

```
brew install ocaml
brew install opam
```

Then do
```
opam init
eval `opam config env`
```

## Install ocamlfind

```
opam install ocamlfind
```

### Install the Cairo Ocaml bindings

https://github.com/Chris00/ocaml-cairo

```
opam install cairo2
```

### Install js_of_ocaml

```
opam install js_of_ocaml
```


## Building the Logo Interperter

There are two different builds of the interperter:

* A interpreter that runs from the command-line and uses Cairo as
  backend to output .png files from Logo programs

* A interpreter that runs from Javascript and uses a Canvas backend
  to output graphics from Logo programs in the browser

## Building the Command-line Interpreter

Issuing the following command

```
make
```

will build and run a program called `logo.native`. It creates
a file named `tree.png` that should contain a tree.

To clean up do
```
make clean
```

## Building the Javascript Interpreter

Issuing the following command

```
make logoweb.js
```

will build a Javascript library containing the interpreter. The file
`index.html` shows how this library may be used from Javascript via
the `interpetLOGO` command.


#### Logo Grammar

I made my own up. But there is one [here](https://www.cs.duke.edu/courses/spring00/cps108/projects/slogo/slogo.g)
