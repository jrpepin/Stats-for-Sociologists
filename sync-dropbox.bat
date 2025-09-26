@echo off
setlocal

REM === CONFIGURATION ===
set WORKTREE_PATH=C:\Users\Joanna\Dropbox\Teaching\Statistics\SOC6302\SOC6302-repo-dropbox
set MAIN_REPO_PATH=C:\Users\Joanna\Documents\GitHub\Stats for Sociologists
set LOG_PATH=C:\Logs\mirror_sync_log.txt

REM === LOG START ===
echo === Sync started on %DATE% at %TIME% === >> "%LOG_PATH%"

REM === CHECK: Worktree path exists ===
if not exist "%WORKTREE_PATH%" (
    echo ERROR: Worktree path not found: %WORKTREE_PATH% >> "%LOG_PATH%"
    echo ERROR: Worktree path not found: %WORKTREE_PATH%
    exit /b
)

REM === CHECK: Main repo path exists ===
if not exist "%MAIN_REPO_PATH%" (
    echo ERROR: Main repo path not found: %MAIN_REPO_PATH% >> "%LOG_PATH%"
    echo ERROR: Main repo path not found: %MAIN_REPO_PATH%
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

REM === STEP 2: Change to worktree ===
echo Changing directory to worktree: "%WORKTREE_PATH%" >> "%LOG_PATH%"
cd /d "%WORKTREE_PATH%"

REM === STEP 3: Checkout dropbox-mirror branch ===
git checkout dropbox-mirror >> "%LOG_PATH%" 2>&1
IF %ERRORLEVEL% NEQ 0 (
    echo Failed to checkout dropbox-mirror branch >> "%LOG_PATH%"
    exit /b
)

REM === STEP 4: Force dropbox-mirror to match main ===
git fetch origin >> "%LOG_PATH%" 2>&1
git reset --hard origin/main >> "%LOG_PATH%" 2>&1
IF %ERRORLEVEL% NEQ 0 (
    echo Reset failed — check for local changes or conflicts >> "%LOG_PATH%"
    exit /b
)

REM === STEP 5: Force push dropbox-mirror to GitHub ===
git push origin dropbox-mirror --force >> "%LOG_PATH%" 2>&1
IF %ERRORLEVEL% NEQ 0 (
    echo Force push failed >> "%LOG_PATH%"
    exit /b
)

REM === LOG END ===
echo === Sync completed on %DATE% at %TIME% === >> "%LOG_PATH%"
