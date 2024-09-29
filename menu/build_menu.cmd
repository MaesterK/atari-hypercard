@echo off

rem defaults
set project=menu
set tools=tools-windows-x86
set dasm=%tools%\dasm.exe
set stella=%tools%\Stella.exe

rem run assembler
echo "assemble: %project%.asm -> %project%.bin"
%dasm% %project%.asm -f3 -o%project%.bin -s%project%.sym

rem run emulator
echo "run: %project%.bin"
%stella% %project%.bin

