# Make your own Logo Interperter


## Install OCAML

Use the Real World Haskell installation instructions
https://github.com/realworldocaml/book/wiki/Installation-Instructions

## On Linux

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

## On Mac

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

## Install the Cairo Ocaml bindings

https://github.com/Chris00/ocaml-cairo

```
opam install cairo2
```

## Install js_of_ocaml

```
opam install js_of_ocaml
```


## Run the Logo Interperter

Issuing the following command

```
make
```

will build and run a program called `logoturtle.byte`. It creates
a file named `graphics.png` that should contain a star.

To clean up do
```
make clean
```


## Logo Grammar

I made my own up. But there is one [here](https://www.cs.duke.edu/courses/spring00/cps108/projects/slogo/slogo.g)
