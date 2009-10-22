#!/usr/bin/python
#
# Compress the range of the first (integral) elements on lines in a
# CSV file.
#
# Usage:
#   cat input.csv | python CompressRange mapping.txt > output.csv
#

import sys


old2new = {}
unique = 0
fp = open(sys.argv[1], "w")


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
    # write it to the mapping file.
    #
    xNew = old2new.get(x, -1)
    if xNew == -1:
        xNew = unique
        unique = unique + 1
        old2new[x] = xNew

        fp.write("%i\n" % x)

    # Write the line out with the new element.
    #
    sys.stdout.write("%i%s" % (xNew, d))


fp.close()

