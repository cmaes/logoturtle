# Make your own Logo Interperter


## Install OCAML

Use the Real World Haskell installation instructions
https://github.com/realworldocaml/book/wiki/Installation-Instructions

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


## Install the Cairo Ocaml bindings

https://github.com/Chris00/ocaml-cairo

```
opam install cairo2
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
