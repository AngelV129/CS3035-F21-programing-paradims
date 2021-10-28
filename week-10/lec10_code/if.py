a = input("Enter a number, negative numbers always invalid: \n")

if not a.isdigit(): # only works on non-negative numbers
    print("invalid input")
else:
    print("You entered: " + a)
    
    a = int(a)
    if a < 4:
        print("number too low")
    elif a > 100:
        print("number too high")
    else:
        print("good number")
        if a < 5:
            print("number almost too low")