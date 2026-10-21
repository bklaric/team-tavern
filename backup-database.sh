#!/bin/bash
# Emails a gzipped dump of the database through SES, as backup@teamtavern.net,
# a sender the IAM user's policy allows. If any step fails it emails what
# failed instead and exits non-zero, so a night without a backup never looks
# like one with it. What it writes to stderr ends up in the log cron.txt names.
set -Eeuo pipefail
# UTC ISO timestamp for file name.
DATETIME=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
FROM="backup@teamtavern.net"
TO="branimir.klaric.bk@gmail.com"
BOUNDARY="database-backup-$DATETIME"
# SES's own unless overridden, as the server's SES client does.
SES_URL="${AWS_ENDPOINT_URL_SESV2:-https://email.eu-central-1.amazonaws.com}/v2/email/outbound-emails"
WORK=$(mktemp -d)
# Stderr is kept for the failure email and passed on, under the run's time and
# status, as the script exits.
exec 3>&2 2> "$WORK/errors"
trap 'status=$?; { echo "$DATETIME exit $status"; cat "$WORK/errors"; } >&3; rm -rf "$WORK"' EXIT

# Sends the raw email in the file named, its lines ending in CRLF as email's
# do. SES's SendEmail request carries it encoded once more. It goes through a
# file because curl refuses a --data argument this large. Nothing here runs in
# a pipeline's subshell, where a failure would set off the ERR trap there as
# well as here.
send() {
    {
        printf '{"FromEmailAddress":"%s","Destination":{"ToAddresses":["%s"]},"Content":{"Raw":{"Data":"' "$FROM" "$TO"
        base64 -w 0 "$1"
        printf '"}}}'
    } > "$WORK/request"
    # Curl signs the request with the IAM user's key, as the AWS SDK would.
    # SES's answer goes to stderr, so its reason for a refusal is in the log.
    curl --silent --show-error --fail-with-body --max-time 300 --write-out '\n' \
        --aws-sigv4 "aws:amz:eu-central-1:ses" \
        --user "$AWS_ACCESS_KEY_ID:$AWS_SECRET_ACCESS_KEY" \
        --header 'Content-Type: application/json' \
        --data "@$WORK/request" \
        "$SES_URL" >&2
}

headers() {
    printf '%s\r\n' \
        "From: $FROM" \
        "To: $TO" \
        "Subject: $1" \
        "MIME-Version: 1.0" \
        "Content-Type: $2" \
        ""
}

# Emails where the script failed and the stderr so far in place of the backup.
# The stderr names the cause: of a pipeline, the trap sees only the last
# command, which may not be the one that failed. Folding keeps its lines within
# email's limit. If this fails too, the log is all there is.
failed() {
    trap - ERR
    set +eu
    {
        headers "Database backup failed $DATETIME" "text/plain; charset=utf-8"
        {
            printf 'backup-database.sh failed at line %s with status %s.\n\n' "$2" "$1"
            cat "$WORK/errors"
        } | fold -w 900 | sed 's/$/\r/'
    } > "$WORK/failure"
    send "$WORK/failure"
    exit "$1"
}
trap 'failed $? $LINENO' ERR

# Checked here rather than left to set -u, which would exit without the trap.
for name in POSTGRES_USER POSTGRES_DB AWS_ACCESS_KEY_ID AWS_SECRET_ACCESS_KEY; do
    [ -n "${!name:-}" ] || { echo "$name is not set." >&2; false; }
done

# The timeout runs in the container, since docker exec passes no signal on to
# what it runs.
docker exec tt-postgres timeout 1h pg_dump --username "$POSTGRES_USER" "$POSTGRES_DB" \
    | gzip -c > "$WORK/dump.sql.gz"
# pg_dump writes this line near the end of a dump it finished.
gunzip -c "$WORK/dump.sql.gz" | tail -n 20 \
    | grep --quiet --line-regexp -- '-- PostgreSQL database dump complete' \
    || { echo "The dump lacks the line pg_dump ends a finished dump with." >&2; false; }

# A line of text and the dump attached.
{
    headers "Database backup $DATETIME" "multipart/mixed; boundary=\"$BOUNDARY\""
    printf '%s\r\n' \
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
    base64 -w 76 "$WORK/dump.sql.gz" | sed 's/$/\r/'
    printf '%s\r\n' "--$BOUNDARY--"
} > "$WORK/email"
send "$WORK/email"
