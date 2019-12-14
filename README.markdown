# Haskell & Docker Compose

This is a simple example of a Haskell application running in a Docker container
with a database connection. In this particular case, the database also runs in
a Docker container. However in general the database could live anywhere. For
example, you might run the application in Docker but run the database on your
host machine.

Just like the [Haskell & Docker][] example, this does not prevent you from
running the application like normal. Install Stack and Postgres, then you can
get going with:

``` sh
> stack setup
> stack build
> stack exec counter
```

And if you want to run the application in Docker but Postgres on your machine,
get Docker and do:

``` sh
> docker build . --tag counter
> docker run \
  --env DATABASE='host=localhost user=postgres' \
  --interactive \
  --publish-all \
  --tty \
  counter
```

But neither of those approaches are new. You can avoid setting up both Haskell
and Postgres on your machine by using Docker Compose. It will build the
necessary containers and connect them together. All you need to do is:

``` sh
> docker-compose build
> docker-compose up -d db # Start the DB first because it needs to initialize.
> docker-compose up -d
```

Then go to `http://localhost:8080`. :tada:

The only difference between this example and the last one is the introduction
of `docker-compose.yml`. It tells the `docker-compose` command how these
containers should be built and how they relate to each other.

[Haskell & Docker]: https://gist.github.com/tfausak/c1932ebaeb0cb13a22d4fe5573da1699
