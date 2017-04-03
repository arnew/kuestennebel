#!/bin/bash -x

file=$1
target=$2
dirname=$(realpath $(dirname ${file}))
basename=$(basename ${file})

IDAPATH=/opt/ida-6.8/
IDA=${IDAPATH}idaq
I64=${IDAPATH}idaq64

kuestennebel=~/kuestennebel

#check idb/i64, call IDA otherwise
idapro=${I64}
idaext=idb
idb=${dirname}/${basename}.${idaext}
json=${dirname}/${basename}.json
names=${dirname}/${basename}.names
echo "x${idb}x ?=? x${file}x"
if [ "x${idb}x" != "x${file}x" ]; then
	${idapro} -B -o${idb} ${file}
#extract names
objdump -Ct ${file} | awk 'BEGIN {file=""; print "[{"; OFS=","; ORS = "},\n{"; } /l    df/ {file = $6; } / (\.data|\.rodata|\.bss)/ {file = ""}/F \.text/ {print "\"function\":\"" $NF "\"","\"file\":\"" file "\"";} END{OFS="";ORS="";print "}]";}' - | tee ${names}
fi

#extract json
${idapro} -A -S"${kuestennebel}/extract_json.py ${json}" ${idb}

${kuestennebel}/plotter.R ${idb} ${json} ${names} ${target}


