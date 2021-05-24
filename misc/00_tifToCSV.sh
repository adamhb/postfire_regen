#!/bin/sh
inFile=$1
bands=$2

for i in $(seq $bands); do  

gdal_translate -b $i -of XYZ $inFile t$i.txt 
 
awk ' !/nan/ {print $3}' t$i.txt > t${i}c.txt

rm t$i.txt

echo "band $i done"

done

gdal_translate -b 1 -of XYZ $inFile XY.txt
awk '!/nan/ {print $1}' XY.txt > tXc.txt 
awk '!/nan/ {print $2}' XY.txt > tYc.txt 

paste t*c.txt > $inFile.csv  
rm t*c.txt
