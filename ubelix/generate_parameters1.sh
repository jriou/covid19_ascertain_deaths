#!/bin/bash

# Sweep
rxmin=1
rxmax=1
rxstep=1

filename="parameters1.txt"

echo "Study: sweeping rx and ry parameters "
echo "   - rx from $rxmin to $rxmax with $rxstep increment"

echo "Saving parameters in ${filename}"

rm -f ${filename}

for ((rx=$rxmin;rx<=$rxmax;rx=$rx+$rxstep)); do    		 
 echo "$rx" >> ${filename} 
done

Nlines=$(grep -c "" < ${filename})
echo "$Nlines different parameters combinations"
