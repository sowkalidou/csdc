export DATABASE_URL=postgresql://csdc:csdc@localhost:5432/csdc
export SECRET_ORCID_ID=$(cat ../secret.json | jq -r .orcidId)
export SECRET_ORCID_SECRET=$(cat ../secret.json | jq -r .orcidSecret)
docker run --network="host" --rm \
  -e DATABASE_URL -e SECRET_ORCID_ID -e SECRET_ORCID_SECRET \
  csdc-dao

