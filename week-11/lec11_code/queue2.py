# ONLY import deque objects from collections module
from collections import deque

# use the imported objects, type deque
d = deque(["Eric", "John", "Michael"])
print(d)

d.append("Terry")
d.append("Graham")
print(d)

d.popleft()
d.popleft()
print(d)