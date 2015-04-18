import fileinput
import sys

for s in fileinput.input():
	a=s.strip().split(' ')
	print a[-1]
	a=a[:-1]
	n=0
	for x in a:
		n+=1
		if x[0]=='{':
			break
		if n%3==1:
			continue
		sys.stdout.write(x + ' ')
	sys.stdout.write('\n')