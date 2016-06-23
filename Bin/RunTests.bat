deskew -t a -a 5 -o TestOut/Outa1.tif ../TestImages/1.tif
deskew -t a -a 10 -o TestOut/Outa2.png ../TestImages/2.png
deskew -t a -a 10 -o TestOut/Outa3.png ../TestImages/3.png
deskew -t a -a 10 -o TestOut/Outa4.png ../TestImages/4.png
deskew -t a -a 10 -o TestOut/Outa5.png ../TestImages/5.png
deskew -t a -a 5 -o TestOut/OutaF1550.jpg ../TestImages/F1550.jpg
deskew -t a -a 5 -b FFFFFF -o TestOut/tiff-jpeg.tif ../TestImages/tiff-jpeg.tif

deskew -t 128 -a 5 -o TestOut/Oute1.tif ../TestImages/1.tif
deskew -t 128 -a 10 -o TestOut/Oute2.png ../TestImages/2.png
deskew -t 180 -a 5 -o TestOut/OuteF1550.jpg ../TestImages/F1550.jpg

deskew -b FF0000 -o TestOut/Outb1.tif ../TestImages/1.tif
deskew -b 00FFFF -o TestOut/Outb5.png ../TestImages/5.png

deskew -r 214,266,933,1040 -o TestOut/Outr4.png ../TestImages/4.png

deskew -t 100 -a 11 -b aa55cc -r 314,366,833,940 -s sp -o TestOut/Outs4.png ../TestImages/4.png

deskew -t a -a 5 -f b1 -o TestOut/Outf1.tif ../TestImages/1.tif
deskew -t a -a 10 -f b1 -o TestOut/Outf2.png ../TestImages/2.png

deskew -t a -a 5 -l 2 -o TestOut/Outs1.tif ../TestImages/1.tif 
