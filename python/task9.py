for a in range(1, 334):
    for b in range(a, 1000-a):
        c = 1000 - a - b
        if c**2 == a**2 + b**2:
            print(a*b*c)

