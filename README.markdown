# Egg API

This is a test app for a simple-as-possible Event Sourcing API using Postgres. It uses one big lump of state, one Action sum type, and one projection that works it all out. Basically, it's the Redux architecture but as a sloppy backend architecture.

Sound interesting? No, me neither. Anyway, to give it a smash, run this:

``` sh
> docker-compose build
> docker-compose up -d
```

Then go to `http://localhost:8080`. :tada:

Thanks @tfausak for the Docker compose template I stole to make this work because I am shit at devops. 

[Haskell & Docker]: https://gist.github.com/tfausak/c1932ebaeb0cb13a22d4fe5573da1699
