#!/bin/bash
# UTC ISO timestamp for file name.
DATETIME=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
# Dump database, encode it in base64 and store it in variable.
BACKUP_BASE64=$(docker exec postgres pg_dump | base64 -w 0)
# Prepare JSON body for Sendgrid API.
DATA='{"personalizations": [{"to": [{"email": "branimir.klaric.bk@gmail.com"}]}],"from": {"email": "backup@teamtavern.net"},"subject":"Database backup '$DATETIME'","content": [{"type": "text/plain","value": "Database backup."}], "attachments": [{"content": "'$BACKUP_BASE64'", "type": "application/sql", "filename": "'$DATETIME'-database-backup.sql"}]}'
# Write JSON body into a file.
# We're doing this because curl complains if we pass it a large --data argument.
# However, it can read --data from files just fine.
echo $DATA > ~/database-backup-body
# Call the Sendgrid API.
curl --request POST \
    --url https://api.sendgrid.com/v3/mail/send \
    --header "authorization: Bearer $SENDGRID_API_KEY" \
    --header 'Content-Type: application/json' \
    --data @${HOME}/database-backup-body
