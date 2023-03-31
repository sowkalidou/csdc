# CSDC DAO

![build](https://github.com/guaraqe/csdc/workflows/build/badge.svg)

This repository contains a work-in-progress webapp for the CSDC DAO. It
currently has two components: a server and a GUI.

## Development

This projects uses [Nix](https://nixos.org/download.html) to manage
dependencies. To install Nix, run:

```
curl -L https://nixos.org/nix/install | sh
```

Then, enter the development shell with:

```
nix-shell
```

which will download all necessary dependencies. Most common commands are
accessible from the Makefile. To list the available commands, run `make`.

In particular:

  - For faster Haskell development, there are many `ghcid-*` targets.

  - For Elm development, there is `make gui-build`.

## Running the server

The first time, deploy the database and ipfs service with:

```
make docker
```

First, make sure the GUI is built with:

```
make gui-build
```

Finally, run:

```
make serve
```

and the server should be available at `localhost:8080`.

# Setting up Fly.io

Make sure to create an account with access to the free tier.

The following commands should be executed inside the Nix shell, by running
`nix-shell` before starting.

First, to login, run:

```console
$ flyctl auth login
```

Then, the Postgres database must be created:

```console
$ flyctl postgres create
```

This command will show the credentials, which should be saved in a `secrets.json` file as:

```json
{
  "pgstring": "postgresql://user:password@host:port"
}
```

Finally, create your app:

```console
$ flyctl launch
```

In this step you will choose the app name, which will determine its URL. This name should be used for the `tag-and-push-image`

In the following, the app name will be called `$APP_NAME`.

Make dure the image is in the `fly.toml` configuration file:

```toml
[build]
  image = "registry.fly.io/$APP_NAME:latest"
```

Save this name in the `secrets.json` file:

```json
{
  "app_name": "$APP_NAME",
  "pgstring": "postgresql://user:password@host:port"
}
```


# Building and deploying

Once more, all commands should be run inside the Nix shell, by running
`nix-shell` before starting.

First, build the docker image and load it:

```console
$ just build-and-load-image
```

Tag the image conveniently, and push it:

```console
$ just tag-and-push-image $APP_NAME
```

And finally, deploy:

```console
$ just deploy $APP_NAME
```

