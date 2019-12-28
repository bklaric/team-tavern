#!/bin/bash
spago bundle-app --main TeamTavern.Job.ConfirmEmailMain --to dist-job/confirm-email-job.js --no-install --no-build
spago bundle-app --main TeamTavern.Job.CreateProfileMain --to dist-job/create-profile-job.js --no-install --no-build
spago bundle-app --main TeamTavern.Job.UpdateProfileMain --to dist-job/update-profile-job.js --no-install --no-build

# node dist-job/confirm-email-job.js
# node dist-job/create-profile-job.js
# node dist-job/update-profile-job.js
