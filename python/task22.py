with open("0022_names.txt") as f:
    names = f.read()
names = names.replace('"', "").split(',')
names.sort()
s = 0
for i,name in enumerate(names):
    score = 0
    for c in name:
        score += (ord(c) - ord('A'))
    score *= (i+1)
    s += score
print(s)