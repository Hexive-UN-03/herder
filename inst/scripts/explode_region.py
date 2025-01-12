import sys
# invoke with python explode_region.py region shapnel
# prints a space-separated string of regions contained within the parent region exploded into shrapnel pieces to console (defaults to 16)
parent_region = sys.argv[1]
if len(sys.argv) > 2:
    shrapnel = int(sys.argv[2])
else:
    shrapnel = 16

# take just the number components
parent_range = parent_region.split(":")
region_title = parent_range[0] + ":"
parent_range = parent_range[1].split("-")

total_length = int(parent_range[1]) - int(parent_range[0])
# if the user wants a region less than the shapnel number tell them to leave (politely)
if total_length >= shrapnel and shrapnel > 1:
    shrapnel_size = total_length // (shrapnel-1)
    end_shrapnel = total_length % (shrapnel-1)
    current_pos = int(parent_range[0])
    fragments = []
    for i in range(0,(shrapnel-1)):
        # -1 to the second term because the range is inclusive
        fragment = region_title + str(current_pos) + "-" + str(int(current_pos)+int(shrapnel_size)-1)
        current_pos += shrapnel_size
        fragments.append(fragment)
    # tack on the end shrapnel
    fragments.append(region_title + str(current_pos) + "-" + str(current_pos+end_shrapnel))
    print(" ".join(fragments), end="")
else:
    print(parent_region, end="")