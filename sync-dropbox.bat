@echo off
setlocal

REM === CONFIGURATION ===
set MAIN_REPO_PATH=C:\Users\Joanna\Documents\GitHub\Stats-for-Sociologists
set DROPBOX_CLONE_PATH=C:\Users\Joanna\Dropbox\Teaching\Statistics\SOC6302\SOC6302-repo-dropbox
set LOG_PATH=C:\Logs\mirror_sync_log.txt

REM === LOG START ===
echo === Sync started on %DATE% at %TIME% === >> "%LOG_PATH%"

REM === CHECK: Main repo path exists ===
if not exist "%MAIN_REPO_PATH%" (
    echo ERROR: Main repo path not found: %MAIN_REPO_PATH% >> "%LOG_PATH%"
    echo ERROR: Main repo path not found: %MAIN_REPO_PATH%
    exit /b
)

REM === CHECK: Dropbox clone path exists ===
if not exist "%DROPBOX_CLONE_PATH%" (
    echo ERROR: Dropbox clone path not found: %DROPBOX_CLONE_PATH% >> "%LOG_PATH%"
    echo ERROR: Dropbox clone path not found: %DROPBOX_CLONE_PATH%
    exit /b
)

REM === STEP 1: Update main repo from GitHub ===
echo Updating main repo from GitHub >> "%LOG_PATH%"
cd /d "%MAIN_REPO_PATH%"
git pull origin main >> "%LOG_PATH%" 2>&1
IF %ERRORLEVEL% NEQ 0 (
    echo Failed to pull latest changes into main repo >> "%LOG_PATH%"
    exit /b
)

REM === STEP 2: Sync Dropbox clone to match main repo ===
echo Syncing Dropbox clone to match main repo >> "%LOG_PATH%"
cd /d "%DROPBOX_CLONE_PATH%"
git checkout main >> "%LOG_PATH%" 2>&1
git pull origin main >> "%LOG_PATH%" 2>&1
IF %ERRORLEVEL% NEQ 0 (
    echo Failed to sync Dropbox mirror branch >> "%LOG_PATH%"
    exit /b
)

REM === STEP 3: Push Dropbox mirror branch to GitHub ===
echo Pushing Dropbox mirror branch to GitHub >> "%LOG_PATH%"
cd /d "%DROPBOX_CLONE_PATH%"
git push origin dropbox-mirror --force >> "%LOG_PATH%" 2>&1
IF %ERRORLEVEL% NEQ 0 (
    echo Failed to push Dropbox mirror branch to GitHub >> "%LOG_PATH%"
    exit /b
)

REM === LOG END ===
echo === Sync completed on %DATE% at %TIME% === >> "%LOG_PATH%"
