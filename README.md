# service.routing [![Build Status](https://travis-ci.org/hiposfer/service.routing.svg?branch=master)](https://travis-ci.org/hiposfer/service.routing)

A routing engine service using Open Street Map as data source. Its current focus are
flexibility, speed and stability in that order.

## Usage

`service.routing` creates an API server with a Swagger interface for your convenience.
You can find it at [localhost:3000](http://localhost:3000). For a quick test, you can
check [try.hyposfer.com](http://try.hiposfer.com/index.html)

### Production

- locally


    $ lein uberjar
    $ java -jar target/routing.jar


- With Docker

Grab the latest docker image and run it:

    $ docker run -it -p 3000:3000 hiposfer/service.routing


### Development

We follow [Stuart Sierra's reload workflow](https://github.com/stuartsierra/component).
The system configuration is created [here](https://github.com/hiposfer/service.routing/blob/master/src/hiposfer/service/routing/core.clj#L77)
by merging the [default config](https://github.com/hiposfer/service.routing/blob/master/resources/default-config.edn)
with the [required environmental variables](https://github.com/hiposfer/service.routing/blob/master/src/hiposfer/service/routing/core.clj#L84).
It is also possible to overwrite the configuration by passing an extra map to
the `config` function ([example](https://github.com/hiposfer/service.routing/blob/master/src/hiposfer/service/routing/dev.clj#L15)).

- locally

    1. start a REPL
    2. load the [dev.clj](https://github.com/hiposfer/service.routing/blob/master/src/hiposfer/service/routing/dev.clj) file.
    3. run the [reset](https://github.com/hiposfer/service.routing/blob/master/src/hiposfer/service/routing/dev.clj#L44) function.
    4. perform your changes
    5. repeat from iii until you are satisfied with your changes.
    
OSM files are usually very large and consume a lot of memory to process, therefore
we use a fake [network](https://github.com/hiposfer/service.routing/blob/master/src/hiposfer/service/routing/core.clj#L43)
for development. The fake `network`'s size can be tuned via the [:network-size](https://github.com/hiposfer/service.routing/blob/master/src/hiposfer/service/routing/core.clj#L43)
configuration. 

Using a fake network can significantly increase your development speed as there is
no need to reparse the OSM file. You can test your changes on a real OSM file by
setting `:dev` to `false`.

- with Docker

build a docker image locally out of your changes:

    $ docker build -t somename .

Or using docker-compose within the source directory:

    $ docker-compose up

---
Distributed under LGPL v3
