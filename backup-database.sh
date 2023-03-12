#!/bin/bash
# UTC ISO timestamp for file name.
DATETIME=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
# Dump database and save the file to disk.
docker exec postgres pg_dump -f /backups/$DATETIME-database-backup.sql
# Encode database backup in base64 and store it in variable.
BACKUP_BASE64=$(cat $POSTGRES_BACKUP_PATH/$DATETIME-database-backup.sql | base64)
# Prepare JSON for POST body to Sendgrid API.
DATA='{"personalizations": [{"to": [{"email": "branimir.klaric.bk@gmail.com"}]}],"from": {"email": "backup@teamtavern.net"},"subject":"Database backup '$DATETIME'","content": [{"type": "text/plain","value": "Database backup."}], "attachments": [{"content": "'$BACKUP_BASE64'", "type": "application/sql", "filename": "'$DATETIME'-database-backup.sql"}]}'
# Create a temp file for JSON body.
TEMP=$(mktemp database-backup-temp-XXXXXXXXX)
# Write JSON body into temp file.
# We're doing this because curl complains if we pass it a large --data argument.
# However, it can read --data from files just fine.
echo $DATA > $TEMP
# Call the Sendgrid API.
curl --request POST \
    --url https://api.sendgrid.com/v3/mail/send \
    --header "authorization: Bearer $SENDGRID_API_KEY" \
    --header 'Content-Type: application/json' \
    --data @$TEMP
