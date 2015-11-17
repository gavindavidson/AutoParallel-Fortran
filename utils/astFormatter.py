import sys

filename = sys.argv[1]

input_file = open(filename, 'r')
file_string = input_file.read()
currentTab = 0

for index in range(0, len(file_string)):
	ch = file_string[index]
	if ch == "(": 
		currentTab += 1
		if file_string[index + 1] != ")":
			print "\n",
			print "\t|"*currentTab,
	elif ch == ")":
		currentTab -= 1
	sys.stdout.write(ch)