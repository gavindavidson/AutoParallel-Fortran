from subprocess import *
import sys

# Simple script to determine a number of lines and a percentage of lines that have been changed between original source code
# and output host code. An attempt to estimate how much of the code was parallelised

def getOriginalLines():
	originalFilename = sys.argv[1]
	lines = Popen("cat " + originalFilename + " | wc -l", stdout=PIPE, shell=True).stdout.read()
	return float(lines)

def getSameLines():
	originalFilename = sys.argv[1]
	newFileName = sys.argv[2]
	lines = Popen("fgrep -x -f " + originalFilename + " " + newFileName + " | wc -l", stdout=PIPE, shell=True).stdout.read()
	return float(lines)

def percentageChange():
	original = getOriginalLines()
	matching = getSameLines()
	percentageChange = ((original - matching) / original)*100
	return round(percentageChange,2)

def absoluteChange():
	original = getOriginalLines()
	matching = getSameLines()
	absoluteChange = original - matching
	return int(absoluteChange)

def loopChange():
	original = countDos(sys.argv[1])
	new = countDos(sys.argv[2])
	percentageChange = ((original - new) / float(original))*100
	return round(percentageChange,2)

def countDos(filename):
	f = open(filename, 'r')
	line = f.readline()
	doCount = 0
	while (line != ""):
		if (line.strip()[:2]=="do"):
			doCount += 1
		line = f.readline()
	return doCount

print "FILE 1: " + sys.argv[1]
print "FILE 2: " + sys.argv[2]
print "LINES"
print "\tAbsolute Change:\t" + str(absoluteChange()) + " lines"
print "\tPercentage Change:\t" + str(percentageChange()) + "%"
print "LOOPS"
print "\tLoop count in " + sys.argv[1] + ":\t" + str(countDos(sys.argv[1]))
print "\tLoop count in " + sys.argv[2] + ":\t" + str(countDos(sys.argv[2]))
print "\tPercentage of loops changed:\t" + str(loopChange()) + "%"