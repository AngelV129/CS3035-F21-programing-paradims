class MyClass:
    """A simple example class"""
    i = 12345

    def f(self): #self is the instance
        return 'hello world'


class Complex:
	def __init__(self, realpart, imagpart):
		#functions in classes are called methods
		#functions with double underscores before and after their name
		#are called dunder functions, or magic functions
		#They usually aren't called directly, but as the result of some
		#other operation, or the use of an operator like +, -, etc.
		self.r = realpart
		self.i = imagpart


class Dog:
    kind = 'canine'         # class variable shared by all instances
    def __init__(self, name):
        self.name = name    # instance variable unique to each instance
        #self is the instance, pass it into each method in the def, but not
        #when using it
        self.bones = 10

    def bury_bones(self):
    	print("Burying {} bones".format(self.bones))

#Creating an instance of Dog using the constructor
my_dog = Dog("Rotty")

#to use (call) a method, just put a dot in front of the instance's variable, then
#put the method there
my_dog.bury_bones()
print(my_dog.kind)
print(my_dog.bones)