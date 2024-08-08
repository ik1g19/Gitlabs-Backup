#!/bin/bash

#counts the words in the output of lineCount
wordCount=$(./lineCount.sh | wc -w)
echo $wordCount
