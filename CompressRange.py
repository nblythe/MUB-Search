#!/usr/bin/python
#
# Compress the range of the first (integral) elements on lines in a
# CSV file.
#
# Usage:
#   cat input.csv | CompressRange mapping.txt > output.csv
#

import sys


old2new = {}
new2old = []


for line in sys.stdin:
    # Split this line around the first comma and convert whatever is before
    # the comma to an integer.
    #
    p = line.find(",")
    if p < 0:
        x = int(line)
        d = ""
    else:
        x = int(line[0 : p])
        d = line[p :]

    # If there isn't a mapping in place for this element, establish one and
    # print out the new line.
    #
    xNew = old2new.get(x, -1)
    if xNew == -1:
        xNew = len(new2old)
        new2old.append(x)
        old2new[x] = xNew

        sys.stdout.write("%i%s" % (xNew, d))


# Write the mapping to a file for later use.
#
fp = open(sys.argv[1], "w")
for m in new2old:
    fp.write("%i\n" % m)
fp.close()

