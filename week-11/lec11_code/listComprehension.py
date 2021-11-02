old_list = [1, 2, 3, 4, 5]

squared = [x**2 for x in old_list]
print(squared)

squared_partial = [x**2 for x in old_list if x>=3]
print(squared_partial)

squared_increment = [x**2 if x>=3 else x+1 for x in old_list]
print(squared_increment)

matrix = [
 [1, 2, 3, 4],
 [5, 6, 7, 8],
 [9, 10, 11, 12],
]
print(matrix)
print([[row[i] for row in matrix] for i in range(4)])