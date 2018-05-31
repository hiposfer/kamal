# kamal [![Build Status](https://travis-ci.org/hiposfer/kamal.svg?branch=master)](https://travis-ci.org/hiposfer/kamal) [![Clojars Project](https://img.shields.io/clojars/v/hiposfer/kamal.svg)](https://clojars.org/hiposfer/kamal)

A routing engine and API service based on Open data. Its current focus are flexibility,
speed and stability in that order.


## Usage
`kamal` creates an API server with a single endpoint, namely `directions`. It comes preloaded with
Swagger interface for playing around with the API. For a demo see [try.hyposfer.com](http://try.hiposfer.com/index.html)
(it will take a few seconds on the first run). By default `kamal` will run at [localhost:3000](http://localhost:3000).

## Installation
You can install `kamal` from _clojars_ using _lein_ or use our _docker_ image.

### Leiningen
```
$ git clone https://github.com/hiposfer/kamal.git && cd kamal
$ lein with-profile release uberjar
$ java -jar target/kamal.jar
```

### Docker
Use our latest docker image:

    $ docker run -it -p 3000:3000 hiposfer/kamal

## Development
We follow [Stuart Sierra's reload workflow](https://github.com/stuartsierra/component).
Our system configuration is created by reading the environment variables. An example
of such config can be found in `hiposfer.kamal.dev/env`. As you can see there, it is
also possible to create a system by providing the configuration directly.

1. Start a REPL
2. Load the `hiposfer.kamal.dev` namespace
3. Run the `hiposfer.kamal.dev/refresh!` function
4. Perform your changes
    - if your changes modify the network build process. Repeat from `3.` 
    - keep making your changes in the repl otherwise

In other words:
```
$ git clone https://github.com/hiposfer/kamal.git && cd kamal
$ lein repl
hiposfer.kamal.core=> (use 'hiposfer.kamal.dev)
hiposfer.kamal.core=> (hiposfer.kamal.dev/refresh!)
```
And then browse to [localhost:3000](http://localhost:3000)

## OSM Files
`kamal` consumes Open Street Map (OSM) data files. OSM files are usually very large and
consume a lot of memory to process, therefore we use a fake `network` during the development.
The fake `network`'s size can be tuned via the `:network-size` configuration. 

Using a fake network can significantly increase your development speed as there is
no need to reparse the OSM file. You can test your changes on a real OSM file by
setting `:dev` to `false`.

`kamal` is very routing oriented, thus no filtering is performed when reading
OSM files. Dealing with unnecessary information in OSM files is left to the
developer. If you dont want to create your own pre-processing script, we recommend
you to use `Òverpass-Api`. [Here](resources/osm/overpass-api-query.txt) is an example
query that we use to get only `pedestrian` relevant paths. You can customize it
however you want; once you are done, click `Export` and get the
`raw data directly from Overpass API` link; with it you can execute the query on a
terminal or script.

## License
`kamal` is distributed under LGPL v3
