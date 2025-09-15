@echo off
cd "C:\Users\Joanna\Dropbox\Teaching\Statistics\SOC6302\SOC6302-repo-dropbox"
git checkout dropbox-mirror
git fetch origin
git merge origin/main
git push origin dropbox-mirror