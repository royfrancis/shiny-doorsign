#!/bin/sh

output=$1

cp -r _extensions ${output}
cp -r fonts ${output}
cp -r www ${output}

cp *.r ${output}
cp *.qmd ${output}
