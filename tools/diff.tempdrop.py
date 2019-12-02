import collections
import difflib
import os
import sys

#----

path1=sys.argv[1]
path2=sys.argv[2]
suffix1=sys.argv[3]
suffix2=sys.argv[4]

#----

files1=list()
for item in os.listdir(path1):
    if suffix1 in item:
        files1.append(item.strip(suffix1))
files2=list()
for item in os.listdir(path2):
    if suffix2 in item:
        files2.append(item.strip(suffix2))
files=files1+files2

counts=collections.Counter(files)
dupids=[item for item in files if counts[item]>1]

for item in dupids:
    file1=os.path.join(path1,'%s%s'%(item,suffix1))
    file2=os.path.join(path2,'%s%s'%(item,suffix2))
    with open(file1,'r') as f1:
        with open(file2,'r') as f2:
            diff=difflib.unified_diff(f1.readlines(),f2.readlines(),\
                fromfile='f1',tofile='f2',)
            iterable=diff
            _exhausted=object()
            if next(iterable,_exhausted)!=_exhausted:
                sys.stdout.write('File 1 = %s\n'%file1)
                sys.stdout.write('File 2 = %s\n'%file2)
                for line in diff:
                    sys.stdout.write(line)
                sys.stdout.write('\n')
