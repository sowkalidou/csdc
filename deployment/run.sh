export DATABASE_URL=postgresql://csdc:csdc@localhost:5432/csdc
docker run --network="host" --rm \
  -e DATABASE_URL \
  csdc-dao

