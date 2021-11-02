# import collections module
import collections

# use something from the imported module, type collections.deque
d = collections.deque(["Eric", "John", "Michael"])
print(d)

d.append("Terry")
d.append("Graham")
print(d)

d.popleft()
d.popleft()
print(d)