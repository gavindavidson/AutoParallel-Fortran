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

def clusterChange():
	original = countDoClusters(sys.argv[1], False)
	new = countDoClusters(sys.argv[2], True)
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

def countDoClusters(filename, reductionCheck):
	f = open(filename, 'r')
	line = f.readline()
	clusterCount = 0
	scopeLevel = 0
	while (line != ""):
		if (line.strip()[:2]=="do"):
			scopeLevel += 1
		elif (line.strip()[:6]=="end do"):
			scopeLevel -= 1
			if (scopeLevel == 0):
				clusterCount += 1
		elif (reductionCheck and line.strip()[:12]=="call reduce_"):
			clusterCount -= 1
		line = f.readline()
	return clusterCount

def mapCount(filename):
	f = open(filename, 'r')
	line = f.readline()
	mapCount = 0
	while (line != ""):
		if (line.strip()[:9]=="call map_"):
			mapCount += 1
		line = f.readline()
	return mapCount

def reduceCount(filename):
	f = open(filename, 'r')
	line = f.readline()
	reduceCount = 0
	while (line != ""):
		if (line.strip()[:12]=="call reduce_"):
			reduceCount += 1
		line = f.readline()
	return reduceCount

print "FILE 1: " + sys.argv[1]
print "FILE 2: " + sys.argv[2]
print "LINES"
print "\t" + str(absoluteChange()) + "\tAbsolute line change"
print "\t" + str(percentageChange()) + "%" + "\tPercentage line change"
print "LOOPS"
print "\t" + str(countDos(sys.argv[1])) + "\tLoop count in " + sys.argv[1]
print "\t" + str(countDos(sys.argv[2])) + "\tLoop count in " + sys.argv[2]
print "\t" + str(loopChange()) + "%" + "\tPercentage of loops changed:"
print "LOOP CLUSTERS"
print "\t" + str(countDoClusters(sys.argv[1], False)) + "\tLoop clusters found in " + sys.argv[1]
print "\t" + str(countDoClusters(sys.argv[2], True)) + "\tOld loop clusters found in " + sys.argv[2]
print "\t" + str(clusterChange()) + "%" + "\tPercentage of loop clusters parallelised"
print "KERNELS"
print "\t" + str(mapCount(sys.argv[2])) + "\tCalls to map kernels found in " + sys.argv[2]
print "\t" + str(reduceCount(sys.argv[2])) + "\tCalls to reduce kernels found in " + sys.argv[2]