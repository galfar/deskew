./deskew -t a -a 5 -o TestOut/outa1.png ../TestImages/1.png
./deskew -t a -a 10 -o TestOut/outa2.png ../TestImages/2.png
./deskew -t a -a 10 -o TestOut/outa3.png ../TestImages/3.png
./deskew -t a -a 10 -o TestOut/outa4.png ../TestImages/4.png
./deskew -t a -a 10 -o TestOut/outa5.png ../TestImages/5.png
./deskew -t a -a 5 -o TestOut/outaF1550.png ../TestImages/F1550.jpg

./deskew -t 128 -a 5 -o TestOut/oute1.png ../TestImages/1.png
./deskew -t 128 -a 10 -o TestOut/oute2.png ../TestImages/2.png
./deskew -t 180 -a 5 -o TestOut/outeF1550.png ../TestImages/F1550.jpg

./deskew -b FF0000 -o TestOut/outb1.png ../TestImages/1.png
./deskew -b 00FFFF -o TestOut/outb5.png ../TestImages/5.png

./deskew -r 214,266,933,1040 -o TestOut/outr4.png ../TestImages/4.png

./deskew -t 100 -a 11 -b aa55cc -r 314,366,833,940 -s sp -o TestOut/outs4.png ../TestImages/4.png