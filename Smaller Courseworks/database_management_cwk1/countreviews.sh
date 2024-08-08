#!/bin/bash

for file in $1/*
do
	count=$(grep -o "Content" $file | wc -l)
	hotelName=$(echo $file | sed "s/.dat//" | sed "s|$1/||")
	hotelList="$hotelList$hotelName $count\n"
done

printf "$(printf "$hotelList" | sort -k2 -n -r)\n"
