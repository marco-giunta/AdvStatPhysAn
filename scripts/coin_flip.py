s = open("coin_flip_input.txt","r").read()

l = s.split(", ")

with open("coin_flip_output.txt","w") as fh :
    for n in l :
        fh.write(f'"{n}", ')
