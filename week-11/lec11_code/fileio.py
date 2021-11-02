try:
    f = open("test.txt", encoding = 'utf-8')
    # perform file operations
    print(f.read())
finally:
    f.close()


with open("test.txt", encoding = 'utf-8') as f:
    # perform file operations
    print(f.read())


with open("testwrite.txt",'w',encoding = 'utf-8') as f:
   f.write("my first file\n")
   f.write("This file\n\n")
   f.write("contains three lines\n")


with open("test.txt", encoding = 'utf-8') as f:
    for line in f:
        print(line, end = '')


with open("test.txt", encoding = 'utf-8') as f:
    print(f.readline(), end = '') #this reads one line