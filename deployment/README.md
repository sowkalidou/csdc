# Deployment

## Creating the Docker image

To create the Docker image, do:

```
nix-build
docker load < result
```

To run the image and have the server accessible at `localhost:8080`, run:

```
./run.sh
```

To inspect the Docker image, do:

```
docker run -it csdc-dao sh
```

## Deploying to Heroku

First, log in to Heroku using:

```
heroku login
```

and login into the container registry:

```
heroku container:login
```

Build the Docker image and load it:

```
nix-build
docker load < result
```

Tag the image conveniently, and push it:

```
docker tag csdc-dao registry.heroku.com/csdc-dao-test/web
docker push registry.heroku.com/csdc-dao-test/web
```

and release it with Heroku:

```
heroku container:release -a csdc-dao-test web
```


