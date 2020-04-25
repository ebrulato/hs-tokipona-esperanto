# hs-tokipona-esperanto

The pupose of this project is to provide an automatic translation from Tokipona to Esperanto. As the tokipona is a very ambiguous language, the translation is so only an approximation of the origninal meaning of the writer. But it seems that there are enought similitudes between TokiPona and Esperanto. 

## More than 800 tests

I used the exscellent lessons of [***"jan Pije"***](http://tokipona.net/tp/janpije/okamasona.php) to implement this module. I think that in 80% of the cases, the original meaning is well keeped. I use also __google translate__ to check if the esperanto version is well translated or not and, moost of the time the result is not so bad. 

This version is a first release, and I hope that I will be able to enhance it in the future.

## Main Principles

### Why Esperanto ?

Esperanto is a fantastic and very easy language to learn. I used the adaptability of the esperanto to translate the 123 Tokipona words. Here is an example.

***pona*** means a lot on thing in Tokipona :
* _ADJ_ : good, positive, useful; friendly, peaceful; simple
* _ADV_ : friendly, ...
* _VERB (tr)_ : to fix, to repair 
* _NOM_ : something good, ... 

I choose to translate this word in Esperanto with  [___bon/a___](http://reta-vortaro.de/revo/art/bon.html#bon.0a) :
* bono (nom) : something good
* bona (adj) : good
* bone (adv) : goodly
* bonas (ntr) : to be good 
* bonigas (tr) : to make something good

We loose a few part of the orignal meaning, but most of the time we keep sufficient information to have a good aproximation of the translation. 

### A dictionary 

_jna Pije_ provides us some common structures used in tokipona to express common words:

* _tomo tawa_ : _"a moving builded structure"_ which is often translated as **car** or **vehicle**. 
* _tomo tawa kon_ : _"an aerian moving builded structure"_... *plane*

In the future, one of my intent is to integrate the existing dictionnaries. Currently, this version of the module contains more thant 100 conpound words.

# Installation

The module as been developed in Haskell, so you must follow the installation of [***cabal***](https://www.haskell.org/cabal/) ;)

## Test the module

> $> cabal run tp-eo-testsuite

and you wil have something like :

> Cases: 810  Tried: 810  Errors: 0  Failures: 0

## Compile the CLI version

TODO

## Compile the Web Service version 

TODO

### Deployment on _Google Cloud Run_

TODO

# More

* [License MIT](LICENSE)
* [Change log](CHANGELOG.md)

## TODO List:

* Initialize the CLI
* Integrates Google Translate tool
* Esperanto version of this file
