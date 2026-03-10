@echo off

set SCRIPT_DIR=%~dp0
set LISP=%SCRIPT_DIR%eliza.lisp

echo Building ELIZA: The Session...
echo.

sbcl --no-sysinit --no-userinit ^
 --eval "(push :building *features*)" ^
 --load "%LISP%" ^
 --eval "(sb-ext:save-lisp-and-die \"eliza.exe\" :toplevel #'eliza-session::run :executable t :purify t)"

if exist "eliza.exe" (
 echo.
 echo Done! Binary: eliza.exe
) else (
 echo.
 echo Build failed.
 exit /b 1
)
