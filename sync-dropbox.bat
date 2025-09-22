@echo off
cd "C:\Users\Joanna\Dropbox\Teaching\Statistics\SOC6302\SOC6302-repo-dropbox"

echo Starting Git operations... > C:\Log\git_update_log.txt 2>&1

:: Stage all changes
git add . >> C:\Log\git_update_log.txt 2>&1

:: Commit with a generic message if there are changes
git diff --cached --quiet || git commit -m "Auto-commit before merge" >> C:\Log\git_update_log.txt 2>&1

:: Proceed with merge and push
git checkout dropbox-mirror >> C:\Log\git_update_log.txt 2>&1
git fetch origin >> C:\Log\git_update_log.txt 2>&1
git merge origin/main >> C:\Log\git_update_log.txt 2>&1
git push origin dropbox-mirror >> C:\Log\git_update_log.txt 2>&1

echo Done. >> C:\Log\git_update_log.txt 2>&1