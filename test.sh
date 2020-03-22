#!/usr/bin/env bash

curl \
  --header "Content-Type: application/json" \
  --request POST \
  --data '{"name":"juan"}' \
  http://localhost:8080/person
