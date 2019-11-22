# Better yourself

Help yourself get `better` at programming with interactive tutorials on specific concepts, coding challenges
on various types of problems, and simple projects that can help you practice what you've learned in practical
ways.

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

## Usage (or rather, usage goal)

You can't actually use this right now. It doesn't do anything. This is the vision though:

1.  New developer appears, knowing almost nothing
2.  Their experienced developer friend goes "hey install this program, and it will teach you everything you need to know"
3.  New developer installs `better` and opens his terminal for the first time
4.  The experienced developer friend says "ok, just type `better tutorial` and you'll be on your way"

```
> better tutorial
Hello New Developer, this is how you use the terminal and the "better" command to learn everything
you need to know.

...
... a whole bunch of tutorial stuff here to get comfortable with a command line. We can write it in
... Markdown and integrate with `paper`/`syncat` if they are available to make it look pretty, right?
...

Ok next lets learn a new concept: Hello world. We start this by typing `better concept hello` at
the command prompt.

If you have a specific programming language in mind you want to learn, let us know by adding the `--language`
argument, like this: `better concept hello --language language-name`. Otherwise, we will pick a nice
one for you. Don't stress too much about which language you choose -- almost everything is applicable
to any language.
> better concept hello
Hello world is everyone's first program, and now it will be yours too. This is how it's done...
```

Maybe even further in the future, we can build a normal program that provides a shell pre-configured
with `fish` and `better` so they can get started without the "Experienced developer friend" role being filled.

Also I would like to be able to specify a human language so that people can learn in whatever way they are
most comfortable. The only problem is I don't know those languages, so we'll just go with English for now.

Also, last goal, is don't make it feel like you're using a Haskell program to the user. IDK if that's a thing,
but I feel like it is. Haskell programs usually seem so unfriendly and analytical to me, similar to Haskell
documentation. Even though we are coding in Haskell to learn Haskell, lets go for something Rust quality, yes?

### Goals; tldr

1.  Provide a __beginner friendly__ way to get used to the command line and get started with programming
2.  Build this in a way that it will be __accessible__ to people of all sorts. This includes:
    *   Internationalization (support for translations and such)
    *   A way to provide content without Internet access, so someone could prepare a package and take it to a
        remote place to teach people without such access
3.  Be able to help people not just __learn__ to code, but also to __improve__ at coding. We are targeting beginners
    and experts alike

## How it might work

We will create tutorials, concepts, problems, and projects as *content packs*, which are made available via some
central repository (we can just use some Git repo somewhere for now). The central repository will be curated and
maintained by someone who ensures the quality and accessibility of all officially endorsed content. *Content
developers* can create new content packs and submit them to our repository (pending review) or are completely free
to become *content providers* and host their own repositories filled with whatever types of content they desire.

The `better` application can then query our central repostiory or, with some configuration, other content providers'
repositories, and serve the content to the user. Content packs will follow some structural rules, which will allow
the application to present the content in the way that is most effective for learning the material, as defined by the
content developer.

Hopefully in the long term, we can have officially endorsed content providers, who maintain a comparable level of
quality and accessibility as the offical source, and have their content automatically available without
additional configuration. We could use a structure similar to certificate authorities to allow these endorsements to
be chained and expanded naturally as time goes on. Content providers could even be graded individually, and we then
allow the user to search for content that meets a particular standard. However, we must first get this project working
before this is relevant.


