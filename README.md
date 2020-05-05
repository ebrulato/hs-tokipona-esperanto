# hs-tokipona-esperanto

The pupose of this project is to provide an automatic translation from Tokipona to Esperanto. As the tokipona is a very ambiguous language, the translation is so only an approximation of the origninal meaning of the writer. But it seems that there are enought similitudes between TokiPona and Esperanto. 

## More than 800 tests

I used the excellent lessons of [***"jan Pije"***](http://tokipona.net/tp/janpije/okamasona.php) to implement this module. I think that in 80% of the cases, the original meaning is well keeped. I use also __google translate__ to check if the esperanto version is well translated or not and, moost of the time the result is not so bad. 

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

_jan Pije_ provides us some common structures used in tokipona to express common words:

* _tomo tawa_ : _"a moving builded structure"_ which is often translated as **car** or **vehicle**. 
* _tomo tawa kon_ : _"an aerian moving builded structure"_... *plane*

In the future, one of my intent is to integrate the existing dictionnaries. Currently, this version of the module contains more thant 100 conpound words.

# Installation

The module has been developed in Haskell, so you must follow the installation of [***stack***](https://docs.haskellstack.org/en/stable/README/) ;)

## Test the module

Theses tests cover only the translation part of the module.
The CLI and the Web Service (Servant) are not covered.

> $> stack test

and you wil have something like :

> Cases: 814  Tried: 814  Errors: 0  Failures: 0

## The CLI version

To compile and install the CLI version, 

> $> stack build --copy-bins

We suppose that you have correctly configured your stack environnement. Now you can use the command line interface of the tool. 

> $> hs-tokipona-esperanto-exe jan pona li wile moku

> bona persono volas mangxi

And you can use the dictionary for a better translation.

> $> hs-tokipona-esperanto-exe jan pona li wile moku ***-d*** 

> ***amiko*** volas mangxi

You can also translate some pure Tokipona text with the ***--in*** parameter.

> $> hs-tokipona-esperanto-exe ***--in=./text-sample/lesson19.txt*** -d

Now you can try the Google Translation. You just have to get an API KEY for the Google Translate service and add an environnement variable called API_KEY_GOOGLE_TRANSLATE. Note we use the v2 of the API. 

> $> hs-tokipona-esperanto-exe --in=./text-sample/chewbacca-defense.txt -d ***--lang=en*** 

See a best, but manual translation here : http://tokipona.net/tp/janpije/chewbacca.php

## Compile and start the Web Service version 

To compile and start the Web Server version, 

> $> stack run hs-tokipona-esperanto-servant

You have to wait fo the following display

> Running on port 8081

Then you can use the script *callLocalhostServer.sh* to check that every thing is allright.

```
{"amplekso":170,"versiono":"0.0.22.0"} 
[{"lingva":"eo","vortara":["mi amas vian amikon"],"kruda":["mi amas vian bonan personon"]}]
please check the dest language or ask the administrator
```

### How to get the *swagger.json*

You can fetch the swagger configuration with this request:

> $> curl -X GET http://localhost:8081/swagger.json

***Limlitations*** : currently it doesn't support https call :( , so on the online https://editor.swagger.io/ tool you can't make any call. 

### Deployment on _Google Cloud Run_

TODO

# More

* [License MIT](LICENSE)
* [Change log](CHANGELOG.md)

## TODO List:

* Use a DB on the Cloud
* Use Google Cloud Run
* add environnement variable to update the server name and the port number
* Do the FARU notes
* Integrates Google Translate tool v3
* Esperanto version of this file
