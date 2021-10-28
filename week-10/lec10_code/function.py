# Function examples:
print("\nFunction examples:")
def fib_rec(n):
    if n == 0: return 0
    if n == 1: return 1
    return fib_rec(n-2) + fib_rec(n-1)

def fib(n):
    """ This is docstring of the fib() function. """
    if n == 0: return 0
    if n == 1: return 1
    cache = [0] * (n+1)
    cache[1] = 1
    for i in range(2, len(cache)):
        cache[i] = cache[i - 1] + cache[i - 2]
    return cache[-1]

print(fib_rec(10))
print(fib(2000))


# Return values:
print("\nReturn values:")
def return_true():
    return True


# No return:
print("\nNo return:")
def print_sometin():
    print("l;djkof")
print_sometin()


# docstring:
print("\ndocstring:")
a = return_true()
print(a)
print(fib.__doc__)


# Functions as first-class citizens:
print("\nFunctions as first-class citizens:")
f = fib
print(f(10))


# Set default values:
print("\nSet default values:")
def ask_ok(prompt, retries=4, reminder='Please try again!'):
# keyword arguments can be set to a default
    while True:
        ok = input(prompt)
        if ok in ('y', 'ye', 'yes'): # in asks whether ok is in the tuple
            return True
        if ok in ('n', 'no', 'nop', 'nope'):
            return False
        retries = retries - 1
        if retries < 0:
            raise ValueError('invalid user response') # throws an exception and stops program
        print(reminder)

if ask_ok("Continue? (y/n)"):
    print("Awesome!")
else:
    print("No problem")