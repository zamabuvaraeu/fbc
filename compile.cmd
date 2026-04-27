set FBC_DIR=C:\Program Files (x86)\FreeBASIC-1.10.1-winlibs-gcc-9.3.0
set FBC_EXE_64="%FBC_DIR%\fbc64.exe"
set FBC_EXE_32="%FBC_DIR%\fbc32.exe"
set OPTIONS=-O 3 -Wc -ffunction-sections,-fdata-sections -Wl --gc-sections

set BASE="C:\Programming\FreeBASIC Projects\fbc-modified\src\compiler"
set TMP_OUTPUT=d:\compiler
xcopy %BASE% %TMP_OUTPUT% /E /Y /I /C /R /G /H

%FBC_EXE_64% -g -gen gcc -m fbc -x bin\Debug\x64\fbc64.exe %TMP_OUTPUT%\*.bas
%FBC_EXE_32% -g -gen gcc -m fbc -x bin\Debug\x86\fbc32.exe %TMP_OUTPUT%\*.bas

%FBC_EXE_64% %OPTIONS% -gen gcc -m fbc -x bin\Release\x64\fbc64.exe %TMP_OUTPUT%\*.bas
%FBC_EXE_32% %OPTIONS% -gen gcc -m fbc -x bin\Release\x86\fbc32.exe %TMP_OUTPUT%\*.bas

rmdir /Q /S %TMP_OUTPUT%

copy bin\Release\x64\fbc64.exe "%FBC_DIR%\fbc64_mod.exe"
copy bin\Release\x86\fbc32.exe "%FBC_DIR%\fbc32_mod.exe"