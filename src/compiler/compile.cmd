set FBC_DIR=C:\Programming\FreeBASIC-1.09.0-win64-gcc-9.3.0
set FBC_EXE_64="%FBC_DIR%\fbc64.exe"
set FBC_EXE_32="%FBC_DIR%\fbc32.exe"
set OPTIONS=-O 3 -gen gcc -Wc -ffunction-sections,-fdata-sections -Wl --gc-sections

%FBC_EXE_64% -m fbc -x fbc64.exe -g *.bas
%FBC_EXE_32% -m fbc -x fbc32.exe -g *.bas

%FBC_EXE_64% -m fbc -x fbc64_icase.exe %OPTIONS% *.bas
%FBC_EXE_32% -m fbc -x fbc32_icase.exe %OPTIONS% *.bas