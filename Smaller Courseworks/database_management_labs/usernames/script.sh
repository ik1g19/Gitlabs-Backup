#!/bin/bash
listOfUsers=$(grep -E "*" comp1204students.txt | sed $"s/,//")

while IFS= read -r line ; do
	webCode=$(curl -f -s http://personal.soton.ac.uk/~$line/index.html)

	if [[ $? -eq 3 ]]
	then
		userPageCount="${userPageCount}${line} $(echo $webCode | wc -m)\n"
	fi

done <<< "$listOfUsers"

printf "$(printf "$userPageCount" | sort -k2 -n)"
