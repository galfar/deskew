@rem If you want to run the test with another deskew executable just pass it as 
@rem a parameter e.g. "RunTests.bat deskew64"
@set DESKEW=deskew

@set "param1=%~1"
@setlocal EnableDelayedExpansion
@if defined param1 set DESKEW=%param1%

%DESKEW% ../TestImages/2.png || goto :error
%DESKEW% -t a -a 10 -o TestOut/Out2.png ../TestImages/2.png || goto :error
%DESKEW% -a 10 -o TestOut/Out3.png ../TestImages/3.png || goto :error
%DESKEW% -o TestOut/Out4.png ../TestImages/4.png || goto :error
%DESKEW% -q lanczos -a 10 -o TestOut/Out5.png ../TestImages/5.png || goto :error
%DESKEW% -g c -o TestOut/OutF1550.jpg ../TestImages/F1550.jpg || goto :error
%DESKEW% -t 128 -o TestOut/Oute2.png ../TestImages/2.png || goto :error
%DESKEW% -t 180 -o TestOut/OuteF1550.jpg ../TestImages/F1550.jpg || goto :error
%DESKEW% -q nearest -b 00FFFF -o TestOut/Outb5.png ../TestImages/5.png || goto :error
%DESKEW% -r 214,266,933,1040 -o TestOut/Outr4.png ../TestImages/4.png || goto :error
%DESKEW% -t 100 -a 11 -b aa55cc -r 314,366,833,940 -s sp -o TestOut/Outs4.png ../TestImages/4.png || goto :error
%DESKEW% -f b1 -o TestOut/Outf2.png ../TestImages/2.png || goto :error
%DESKEW% -f rgba32 -b 40ff00ff -o TestOut/Outa6.png ../TestImages/6.png || goto :error
%DESKEW% -f g8 -b 77 -s t -o TestOut/Outg6.png ../TestImages/6.png || goto :error
%DESKEW% -g d ../TestImages/5.png || goto :error

%DESKEW% -t a -a 5 -o TestOut/Out1.tif ../TestImages/1-lzw.tif || goto :error
%DESKEW% -b DD -c j95,tjpeg -o TestOut/Out-tiff-jpeg.tif ../TestImages/tiff-jpeg.tif || goto :error
%DESKEW% -t 128 -c tinput -o TestOut/Out1-g4.tif ../TestImages/1-g4.tif || goto :error
%DESKEW% -b FF0000 -c tdeflate -o TestOut/Out1-deflate.tif ../TestImages/1-lzw.tif || goto :error
%DESKEW% -f b1 -o TestOut/Out1-b1.tif ../TestImages/1-lzw.tif || goto :error  
%DESKEW% -a 5 -l 2 -o TestOut/Outl1.tif ../TestImages/1-lzw.tif || goto :error    

@echo:
@echo [92mTESTS PASSED![0m
@exit /b 0

:error
@echo [91mTESTS FAILED[0m
@exit /b 
