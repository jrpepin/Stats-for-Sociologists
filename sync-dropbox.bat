@echo off
cd "C:\Users\Joanna\Dropbox\Teaching\Statistics\SOC6302\SOC6302-repo-dropbox"

echo Starting Git operations... > C:\Log\git_update_log.txt 2>&1

:: Make sure we're on dropbox-mirror
git checkout dropbox-mirror >> C:\Log\git_update_log.txt 2>&1

:: Stage all changes
git add . >> C:\Log\git_update_log.txt 2>&1

:: Commit if there are staged changes
git diff --cached --quiet || git commit -m "Auto-commit before merge" >> C:\Log\git_update_log.txt 2>&1

:: Fetch and merge from origin/main
git fetch origin >> C:\Log\git_update_log.txt 2>&1
git merge origin/main >> C:\Log\git_update_log.txt 2>&1

:: Push everything to dropbox-mirror
git push origin dropbox-mirror >> C:\Log\git_update_log.txt 2>&1

echo Done. >> C:\Log\git_update_log.txt 2>&1
