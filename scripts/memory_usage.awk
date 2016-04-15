#!/usr/bin/awk -f

BEGIN{total=0;available=0} /^MemTotal/{total=$2} /^MemAvailable/{available=$2} END{printf "Mem: %4.2f%%",available/total*100}
