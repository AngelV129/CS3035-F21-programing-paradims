from random import randrange
import time

def time_it(f, n, arr):
	start = time.time()
	print(f(n, arr))
	end = time.time()
	print(f.__name__, time.strftime("%H:%M:%S", time.gmtime(end-start)))

# find if two values in arr add up to n: can you make this faster?
# [2,1,4, 5]
def slow_function(n, arr):
    for i in range(len(arr)):
    	for j in range(i, len(arr)):
    		if arr[i] + arr[j] == n: return True
    return False

def faster_function(n, arr):
    cache = set()
    for x in arr:
        complement = n-x
        if complement in cache: return True
        cache.add(x)

    return False
big_list = [randrange(10000) for _ in range(1000000)]



time_it(slow_function, 72, big_list)
time_it(faster_function, 72, big_list)