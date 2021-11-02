""" Python Bootcamp 2 """

"""
Topics:
-- Data Structures: Lists + Tuples + Sets + Dictionaries
-- Classes
-- Basic File IO
"""


"""
Resources:
-- Python Tutorial: https://docs.python.org/3/tutorial/index.html
-- Python 3 Documentations: https://docs.python.org/3/
"""




"""
********************************************************************************
I. Data Structures

https://docs.python.org/3/tutorial/datastructures.html


***********************************************************
I-I. Lists

https://docs.python.org/3/tutorial/datastructures.html#more-on-lists

The list data type has some more methods. Here are all of the methods of list
objects:

list.append(x)
-- Add an item to the end of the list. Equivalent to a[len(a):] = [x].

list.extend(iterable)
-- Extend the list by appending all the items from the iterable. Equivalent to
    a[len(a):] = iterable.

list.insert(i, x)
-- Insert an item at a given position. The first argument is the index of the element
    before which to insert, so a.insert(0, x) inserts at the front of the list, and
    a.insert(len(a), x) is equivalent to a.append(x).

list.remove(x)
-- Remove the first item from the list whose value is equal to x. It raises a
    ValueError if there is no such item.

list.pop([i])
-- Remove the item at the given position in the list, and return it. If no index
    is specified, a.pop() removes and returns the last item in the list. (The square
    brackets around the i in the method signature denote that the parameter is
    optional, not that you should type square brackets at that position. You will
    see this notation frequently in the Python Library Reference.)

list.clear()
-- Remove all items from the list. Equivalent to del a[:].

list.index(x[, start[, end]])
-- Return zero-based index in the list of the first item whose value is equal to x.
    Raises a ValueError if there is no such item.
-- The optional arguments start and end are interpreted as in the slice notation
    and are used to limit the search to a particular subsequence of the list.
    The returned index is computed relative to the beginning of the full sequence
    rather than the start argument.

list.count(x)
-- Return the number of times x appears in the list.

list.sort(key=None, reverse=False)
-- Sort the items of the list in place (the arguments can be used for sort
    customization, see sorted() for their explanation).
-- sorted(yourList) gives a new sorted list from your list
    -- https://docs.python.org/3/library/functions.html#sorted

list.reverse()
-- Reverse the elements of the list in place.
-- reversed(yourList) gives you a new reversed list from your list
    -- https://docs.python.org/3/library/functions.html#reversed

list.copy()
-- Return a shallow copy of the list. Equivalent to a[:].


Stacks: Last in first out data structures: use .append() and .pop()
-- https://docs.python.org/3/tutorial/datastructures.html#using-lists-as-stacks
-- see the following example (also in stack.py)
"""
# s = [3, 4, 5]
# s.append(6)
# s.append(7)
# print(s)
#
# s.pop()
# print(s)
#
# s.pop()
# s.pop()
# print(s)

"""
Queues: First in first out data structures, try importing collections
-- https://docs.python.org/3/tutorial/datastructures.html#using-lists-as-queues

Importing modules: you can import a module, put it at the top of the script
-- see the following two examples (also in queue1.py and queue2.py)
"""
# # import collections module
# import collections
#
# # use something from the imported module, type collections.deque
# d = collections.deque(["Eric", "John", "Michael"])
# print(d)
#
# d.append("Terry")
# d.append("Graham")
# print(d)
#
# d.popleft()
# d.popleft()
# print(d)

# # ONLY import deque objects from collections module
# from collections import deque
#
# # use the imported objects, type deque
# d = deque(["Eric", "John", "Michael"])
# print(d)
#
# d.append("Terry")
# d.append("Graham")
# print(d)
#
# d.popleft()
# d.popleft()
# print(d)


"""
***********************************************************
I-II. List Comprehensions

https://docs.python.org/3/tutorial/datastructures.html#list-comprehensions

A quick way to transform a list: we get a new list with transformed values

Exercise: given a list of ints, make a new list where each number is squared
(see listComprehension.py)

A cleaner, more efficient, more readable way:
squared = [x**2 for x in old_list]
# [x^2 | x <- [0..99], x < 5] # Haskell

syntax: [output for element in list]

Adding a condition:
[output for element in list if condition]
if ... else condition:
[output if condition else other_output for element in list]


Nested list comprehension (also in listComprehension.py)
https://docs.python.org/3/tutorial/datastructures.html#nested-list-comprehensions
"""
# matrix = [
#  [1, 2, 3, 4],
#  [5, 6, 7, 8],
#  [9, 10, 11, 12],
# ]
# print(matrix)
# print([[row[i] for row in matrix] for i in range(4)])


"""
***********************************************************
I-III. Tuples

zip(a, b) takes lists a and b and returns a list of tuples of corresponding
elements in lists a and b
-- Note, it truncates the longer of the two lists during the zip

see the following example (also in tuple.py)
"""
# a = list(range(30))
# b = list(range(30, 600))
# c = list(zip(a, b))
# print(c)


"""
***********************************************************
I-IV. The del statement

https://docs.python.org/3/tutorial/datastructures.html#the-del-statement

del a[0] removes the 0th element of a list

see the following example (also in delete.py)
"""
# a = [-1, 1, 66.25, 333, 333, 1234.5]
# print(a)
# del a[0]
# print(a)

"""
***********************************************************
I-V. Sets and Dictionaries

https://docs.python.org/3/tutorial/datastructures.html#sets

https://docs.python.org/3/tutorial/datastructures.html#dictionaries


Finding whether and where an element is in a list will take time proportional to the
length of the list on the worst case, or O(n) time. However, if we knew the index of
the element, we could retrieve it in O(1) time. Hashed data structures do this; they
the value and hash it to an int, and place that in an array at that index. That
way we can look it up in O(1) time.

Two data structures that use hashing: sets and dictionaries.
-- Sets are collections with no repeating values.
-- Dictionaries are (key, value) pairs.

Note the values placed in a set or the keys placed in a dictionary must be immutable.
Strings, tuples, and ints are often used as keys.

see following examples (also in setDictionary.py)

"""
# #creating a set
# basket = {'apple', 'orange', 'apple', 'pear', 'orange', 'banana'}
# print("turtle" in basket)
#
# #creating an empty set
# my_set = set()
#
# #adding a value to a set
# my_set.add(2)
# #removing a value from a set
# my_set.remove(2)
#
# #creating a dictionary
# dct = {1:"word", 2:"ok"}
# #empty dictionary
# empty_dct = {}
#
# #adding a value to a dictionary
# dct[3] = "purple"
#
# #deleting a key from a dictionary
# del dct[3]
#
# #see documentation for all the other functions

"""
Performance comparasion: Set vs List
-- see following examples (also in setVsList.py)
"""
# from random import randrange
# import time
#
# def time_it(f, n, arr):
# 	start = time.time()
# 	print(f(n, arr))
# 	end = time.time()
# 	print(f.__name__, time.strftime("%H:%M:%S", time.gmtime(end-start)))
#
# # find if two values in arr add up to n: can you make this faster?
# # [2,1,4, 5]
# def slow_function(n, arr):
#     for i in range(len(arr)):
#     	for j in range(i, len(arr)):
#     		if arr[i] + arr[j] == n: return True
#     return False
#
# def faster_function(n, arr):
#     cache = set()
#     for x in arr:
#         complement = n-x
#         if complement in cache: return True
#         cache.add(x)
#
#     return False
# big_list = [randrange(10000) for _ in range(1000000)]
#
#
#
# time_it(slow_function, 72, big_list)
# time_it(faster_function, 72, big_list)




"""
********************************************************************************
II. Classes

Classes provide blueprints for objects, these are entities that live in memory
that have:
-- data, either for each individual object or for the whole class
-- behaviors: functions that do things

see the following example (also in class.py)
"""
# class MyClass:
#     """A simple example class"""
#     i = 12345
#
#     def f(self): #self is the instance
#         return 'hello world'
#
#
# class Complex:
# 	def __init__(self, realpart, imagpart):
# 		#functions in classes are called methods
# 		#functions with double underscores before and after their name
# 		#are called dunder functions, or magic functions
# 		#They usually aren't called directly, but as the result of some
# 		#other operation, or the use of an operator like +, -, etc.
# 		self.r = realpart
# 		self.i = imagpart
#
#
# class Dog:
#     kind = 'canine'         # class variable shared by all instances
#     def __init__(self, name):
#         self.name = name    # instance variable unique to each instance
#         #self is the instance, pass it into each method in the def, but not
#         #when using it
#         self.bones = 10
#
#     def bury_bones(self):
#     	print("Burying {} bones".format(self.bones))
#
# Creating an instance of Dog using the constructor
# my_dog = Dog("Rotty")
#
# #to use (call) a method, just put a dot in front of the instance's variable, then
# #put the method there
# my_dog.bury_bones()
# print(my_dog.kind)
# print(my_dog.bones)


"""
Why use classes?
-- Lots of object-oriented patterns use them.
    -- The observer pattern is a common one, where you have one object publishing
        data to a bunch of observer objects.

see the following example (also in observerPattern.py)
"""
# import time
#
#
# class Publisher:
#     def __init__(self):
#         self.subscribers = []
#
#     def subscribe(self, subscriber):
#         self.subscribers.append(subscriber)
#         print("{} has subscribed!".format(subscriber.name))
#
#     def unsubscribe(self, subscriber):
#         self.subscribers.remove(subscriber)
#
#     def publish(self, message):
#         for subscriber in self.subscribers:
#             subscriber.onReceive(message)
#
#
# class Subscriber:
#     def __init__(self, name, publisher):
#         self.name = name
#         self.publisher = publisher
#
#     def subscribe(self):
#         self.publisher.subscribe(self)
#
#     def onReceive(self, message):
#         print("{} received a message: {}".format(self.name, message))
#
#
# n = 0
# publisher = Publisher()
#
# while n < 10:
#     subscriber = Subscriber("Subscriber_" + str(n), publisher)
#     subscriber.subscribe()
#     publisher.publish(subscriber.name + " has joined!")
#     time.sleep(5)
#     n += 1




"""
********************************************************************************
III. Basic File IO

https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files

Reading and writing from a file is simple in Python. One way to do it is:
file = open('path to your file')

Then you can use file to read the contents.

There are also modes for the open function. These are passed as a second parameter.
The default is 'r'.

The Modes
Mode | Description
'r'	Open a file for reading. (default)
'w'	Open a file for writing. Creates a new file if it does not exist or truncates the file if it exists.
'x'	Open a file for exclusive creation. If the file already exists, the operation fails.
'a'	Open for appending at the end of the file without truncating it. Creates a new file if it does not exist.
't'	Open in text mode. (default)
'b'	Open in binary mode.
'+'	Open a file for updating (reading and writing)

Passing in the encoding (recommended)
f = open("test.txt", mode = 'r', encoding = 'utf-8')

If you open a file in the above manner, you have to manually close it with
f.close()

You can make this safer by using a try/except/finally block (also in fileio.py)
"""
# try:
#     f = open("test.txt",encoding = 'utf-8')
#     # perform file operations
#     print(f.read())
# finally:
#     f.close()

"""
If you use with, you don't need to close it (also in fileio.py)
"""
# with open("test.txt",encoding = 'utf-8') as f:
#     # perform file operations
#     print(f.read())

"""
To write to a file, do this: (also in fileio.py)
"""
# with open("testwrite.txt",'w',encoding = 'utf-8') as f:
#    f.write("my first file\n")
#    f.write("This file\n\n")
#    f.write("contains three lines\n")

"""
To read the whole file, use read() with no parameters

To read a file line by line, do this: (also in fileio.py)
"""
# with open("your file path") as f:
#     for line in f:
#         print(line, end = '')

"""
or you can use readline()
(also in fileio.py)
"""
# with open("your file path") as f:
#     print(f.readline(), end = '') #this reads one line
