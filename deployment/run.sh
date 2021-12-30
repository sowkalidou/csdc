export DATABASE_URL=postgresql://csdc:csdc@localhost:5432/csdc
docker run --network="host" --rm \
  -e DATABASE_URL \
  -e GMAIL_SMTP_PORT \
  -e GMAIL_SMTP_SERVER \
  -e GMAIL_SMTP_LOGIN \
  -e GMAIL_SMTP_PASSWORD \
  csdc-dao

