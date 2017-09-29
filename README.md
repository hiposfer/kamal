# kamal [![Build Status](https://travis-ci.org/hiposfer/kamal.svg?branch=master)](https://travis-ci.org/hiposfer/kamal) [![Clojars Project](https://img.shields.io/clojars/v/hiposfer/kamal.svg)](https://clojars.org/hiposfer/kamal)

A routing engine and API service based on Open Street Map data. Its current focus are flexibility, speed and stability in that order.

## Usage
`kamal` creates an API server with a single endpoint, namely `directions`. It comes preloaded with Swagger interface for playing around with the API. For a demo see [try.hyposfer.com](http://try.hiposfer.com/index.html) (it will take a few seconds on the first run). By default `kamal` will run at [localhost:3000](http://localhost:3000).

## Installation
You can install `kamal` from _clojars_ using _lein_ or use our _docker_ image.

### Leiningen
```
$ git clone https://github.com/hiposfer/kamal.git && cd kamal
$ lein uberjar
$ java -jar target/kamal.jar
```

### Docker
Use our latest docker image:

    $ docker run -it -p 3000:3000 hiposfer/kamal

You can also use `docker-compose` if you like:

    $ git clone https://github.com/hiposfer/kamal.git && cd kamal
    $ docker-compose up

## Development
We follow [Stuart Sierra's reload workflow](https://github.com/stuartsierra/component). Our system configuration is created by merging the `resources/default-config.edn` with the passed `environmental variables`. It is also possible to overwrite the configuration by passing an extra map to the `hiposfer.core.config` function.

1. Start a REPL
2. Load the `hiposfer.kamal.dev` namespace
3. Run the `hiposfer.kamal.dev/reset` function
4. Perform your changes
5. Repeat from `3.` until you are satisfied with your changes.

In other words:
```
$ git clone https://github.com/hiposfer/kamal.git && cd kamal
$ lein repl
hiposfer.kamal.core=> (use 'hiposfer.kamal.dev)
hiposfer.kamal.core=> (hiposfer.kamal.dev/reset)
```
And then browse to [localhost:3000](http://localhost:3000). Don't forget to run `(hiposfer.kamal.dev/reset)` after making any change to the source code in order for your changes to take effect.

## OSM Files
`kamal` consumes Open Street Map (OSM) data files. OSM files are usually very large and consume a lot of memory to process, therefore we use a fake `network` during the development. The fake `network`'s size can be tuned via the `:network-size` configuration. 

Using a fake network can significantly increase your development speed as there is
no need to reparse the OSM file. You can test your changes on a real OSM file by
setting `:dev` to `false`.

## License
`kamal` is distributed under LGPL v3
