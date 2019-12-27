## ripser-haskell (v 0.1.0.0)

# Introduction
A wrapper around [ripser](https://github.com/Ripser/ripser) which formats a distance matrix or point-cloud to send to ripser
on stdin and then parses the returned persistence intervals.

From the repo: "Ripser is a lean C++ code for the computation of Vietoris–Rips persistence barcodes. 
It can do just this one thing, but does it extremely well."

Perhaps this will evolve to wrap the ripser code itself as a library we can call with the FFI.  But for now, this will do.

# Installation
This package requires that "ripser" is on your path or that the path is given in the configuration.

Building ripser should be as easy as:

```
git clone https://github.com/Ripser/ripser
cd ripser
make
```

and then you can install it, e.g.,
```cp ripser /usr/local/bin```
