@echo off
rem Task Scheduler entry point: capture_lines.cmd LABEL [extra capture_lines.R arguments]
rem Set CFB_RSCRIPT to override the Rscript path (for example the bundled windows-runtime R).
setlocal
cd /d "%~dp0.."
set "LC_ALL=English_United States.utf8"
if not defined CFB_RSCRIPT set "CFB_RSCRIPT=C:\Program Files\R\R-4.2.2\bin\Rscript.exe"
if not exist "%CFB_RSCRIPT%" set "CFB_RSCRIPT=Rscript"
if not exist "cfb_v3\data\line_captures\logs" mkdir "cfb_v3\data\line_captures\logs"
set "LABEL=%~1"
if "%LABEL%"=="" set "LABEL=adhoc"
shift
set "EXTRA="
:collect
if "%~1"=="" goto run
set "EXTRA=%EXTRA% %1"
shift
goto collect
:run
"%CFB_RSCRIPT%" --vanilla cfb_v3\capture_lines.R --label=%LABEL%%EXTRA% >> "cfb_v3\data\line_captures\logs\capture_lines.log" 2>&1
exit /b %ERRORLEVEL%
