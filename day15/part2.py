from z3 import *

x = Int('x')
y = Int('y')

s = Solver()

s.add(x >= 0, x <= 4000000)
s.add(y >= 0, y <= 4000000)

with open('assets/day15', 'r') as f:
    for line in f:
        words = line.split()
        x1 = int(words[2][2:-1])
        y1 = int(words[3][2:-1])
        x2 = int(words[8][2:-1])
        y2 = int(words[9][2:])
        
        d = abs(x1 - x2) + abs(y1 - y2)
        b1 = y1 - x1
        b2 = y1 + x1
        
        s.add(Or(
            y > (x + b1 + d),
            y < (x + b1 - d),
            y > (-x + b2 + d),
            y < (-x + b2 - d),
        ))

s.check()
m = s.model()
print(f'Part 2: {m[x].as_long() * 4000000 + m[y].as_long()}')
