words = ["at", "to", "howdy", "opie", "total", "wimp", "gong"]

# Print all elements of a list:
print("\nPrint all elements of a list:")
more_than_two = []
for i, w in enumerate(words):
    print(w + " found at index " + str(i))
    if len(w) > 2:
        more_than_two.append(w)

# Print a list:
print("\nPrint a list:")
print(more_than_two)

# Another way of printing all elements:
print("\nAnother way of printing all elements:")
for i in range(len(words)):
    print(words[i])

# Print both index and value:
print("\nPrint both index and value:")
for i, w in enumerate(words):
    print((i, w))   #(i, w) is a tuple, an immutable collection

# Backwards:
print("\nBackwards:")
for w in reversed(words):
    print(w)

#Another way of backwards:
print("\nAnother way of backwards:")
for w in words[::-1]:
    print(w)