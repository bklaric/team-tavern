#!/bin/bash
export DEPLOYMENT=local
export PGUSER=bklaric
export PGPASSWORD=bklaric
export PGHOST=localhost
export PGPORT=5432
export PGDATABASE=team_tavern
export POSTMARK=d763b189-d006-4e4a-9d89-02212ccd87f5

pulp run --main ServerMain
