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

REM === STEP 1: Detach worktree from dropbox-mirror ===
echo Changing directory to worktree: "%WORKTREE_PATH%" >> "%LOG_PATH%"
cd /d "%WORKTREE_PATH%"
git checkout --detach >> "%LOG_PATH%" 2>&1
IF %ERRORLEVEL% NEQ 0 (
    echo Failed to detach worktree >> "%LOG_PATH%"
    exit /b
)

REM === STEP 2: Stage and commit changes ===
git add . >> "%LOG_PATH%" 2>&1
IF %ERRORLEVEL% NEQ 0 (
    echo Failed to stage changes >> "%LOG_PATH%"
    exit /b
)

git commit -m "Auto-sync from Dropbox worktree" >> "%LOG_PATH%" 2>&1
IF %ERRORLEVEL% EQU 1 (
    echo No changes to commit >> "%LOG_PATH%"
) ELSE IF %ERRORLEVEL% NEQ 0 (
    echo Commit failed >> "%LOG_PATH%"
    exit /b
)

REM === STEP 3: Pull latest changes from GitHub ===
git pull origin main --rebase >> "%LOG_PATH%" 2>&1
IF %ERRORLEVEL% NEQ 0 (
    echo Pull (rebase) failed >> "%LOG_PATH%"
    exit /b
)

REM === STEP 4: Push to GitHub ===
git push origin HEAD:main >> "%LOG_PATH%" 2>&1
IF %ERRORLEVEL% NEQ 0 (
    echo Push failed >> "%LOG_PATH%"
    exit /b
)

REM === LOG END ===
echo === Sync completed on %DATE% at %TIME% === >> "%LOG_PATH%"
