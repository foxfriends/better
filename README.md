# Better yourself

Help yourself become a better programmer with interactive tutorials on specific concepts, or coding challenges
on various types of problems.

## Status

This is a very work in progress project. The main goal here is for me to learn Haskell, but if at the end
we end up with a working product that can help others learn new concepts too, then all the better.

## Setup

I don't really know how to set up a Haskell, so go figure that out. This project uses `stack` and
Haskell 8.6.5 or something. I installed it with `asdf`.

Once you have it all set up, the following commands are what you want:

```sh
stack build               # build everything
stack run -- tutorial     # run `better`, passing arguments after `--` as the args to better
```
