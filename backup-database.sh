#!/bin/bash
# Emails a gzipped dump of the database through SES, as backup@teamtavern.net,
# a sender the IAM user's policy allows.
set -o pipefail
# UTC ISO timestamp for file name.
DATETIME=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
FROM="backup@teamtavern.net"
TO="branimir.klaric.bk@gmail.com"
BOUNDARY="database-backup-$DATETIME"
# The raw email: a line of text and the dump attached. Its lines end in CRLF,
# as email's do.
message() {
    printf '%s\r\n' \
        "From: $FROM" \
        "To: $TO" \
        "Subject: Database backup $DATETIME" \
        "MIME-Version: 1.0" \
        "Content-Type: multipart/mixed; boundary=\"$BOUNDARY\"" \
        "" \
        "--$BOUNDARY" \
        "Content-Type: text/plain; charset=utf-8" \
        "" \
        "Database backup." \
        "" \
        "--$BOUNDARY" \
        "Content-Type: application/gzip" \
        "Content-Disposition: attachment; filename=\"$DATETIME-database-backup.sql.gz\"" \
        "Content-Transfer-Encoding: base64" \
        ""
    docker exec tt-postgres pg_dump --username "$POSTGRES_USER" "$POSTGRES_DB" \
        | gzip -c | base64 -w 76 | sed 's/$/\r/'
    printf '%s\r\n' "--$BOUNDARY--"
}
# SES's SendEmail request carries the raw email encoded once more. It goes
# through a file because curl refuses a --data argument this large.
{
    printf '{"FromEmailAddress":"%s","Destination":{"ToAddresses":["%s"]},"Content":{"Raw":{"Data":"' "$FROM" "$TO"
    message | base64 -w 0
    printf '"}}}'
} > ~/database-backup-body
# Curl signs the request with the IAM user's key, as the AWS SDK would.
curl --silent --show-error --fail-with-body \
    --aws-sigv4 "aws:amz:eu-central-1:ses" \
    --user "$AWS_ACCESS_KEY_ID:$AWS_SECRET_ACCESS_KEY" \
    --header 'Content-Type: application/json' \
    --data @${HOME}/database-backup-body \
    https://email.eu-central-1.amazonaws.com/v2/email/outbound-emails
