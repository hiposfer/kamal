# kamal [![Build Status](https://travis-ci.org/hiposfer/kamal.svg?branch=master)](https://travis-ci.org/hiposfer/kamal) [![Clojars Project](https://img.shields.io/clojars/v/hiposfer/kamal.svg)](https://clojars.org/hiposfer/kamal)

A routing engine and API service based on Open data. Its current focus are flexibility,
speed and stability in that order.


## Usage
Check out the [API examples](resources/kamal.postman_collection.json) provided as a [Postman](getpostman.com) collection.
Simply import it and have fun :)

For a demo see [try.hyposfer.com](http://try.hiposfer.com/index.html)
(it will take a few seconds on the first run).
 
By default `kamal` will run at [localhost:3000](http://localhost:3000).

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
`kamal` consumes General Transit Feed Specification (GTFS) and Open Street Map files.

`kamal` is very routing oriented, thus we rely on the awesome Overpass-API for
filtering of Open Street Map data. Check out our `pedestrian` filtering query
[Here](resources/osm/overpass-api-query.txt). 

You can customize it however you want; once you are done, simply save the query run the
preprocessing script again with `lein preprocess resources/`. This will preprocess
all gtfs configs in your environment variables with (automatically) downloaded osm data.

## License
`kamal` is distributed under LGPL v3
