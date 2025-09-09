@echo off
setlocal EnableExtensions EnableDelayedExpansion

rem --- Build with Free Pascal (expects fpc.exe on PATH) ---
fpc.exe pasta
if errorlevel 1 (
  echo Build failed.
  exit /b 1
)

rem --- Clean untracked files (requires Git) ---
git clean -df .
if errorlevel 1 (
  echo git clean failed.
  exit /b 1
)

rem --- Gather version/hash info ---
for /f %%i in ('pasta --version') do set "PASTA_VER=%%i"
for /f %%i in ('git rev-parse --short HEAD') do set "GIT_SHORT=%%i"

rem --- OS/arch labels for Windows ---
set "SYS=Windows"
set "ARCH=%PROCESSOR_ARCHITECTURE%"

rem --- Compose ZIP file name ---
set "ZIP=pasta80-%PASTA_VER%-%GIT_SHORT%-%SYS%-%ARCH%.zip"

rem --- Create ZIP (uses PowerShell Compress-Archive) ---
if exist "%ZIP%" del /f /q "%ZIP%"
powershell -NoProfile -Command ^
  "Compress-Archive -Path 'LICENSE.txt','README.md','pasta.exe','docs','examples','misc','rtl','tests' -DestinationPath '%ZIP%' -Force"
if errorlevel 1 (
  echo Failed to create archive.
  exit /b 1
)

echo.
echo ------------------------------[ Release ready ]---------------------------------
dir /-C "%ZIP%"
echo --------------------------------------------------------------------------------
echo.

endlocal