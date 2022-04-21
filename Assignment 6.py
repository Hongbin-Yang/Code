### Question 1
def polynomial_function(x):
    polynomial = x**1 +1
    for i in range(1,x+1):
        polynomial = polynomial + x ** i-1
    return polynomial

while True:
    x = int(input("Please enter a number between 1-10 to find its polynomial:"))

    if x<0 or x >10:
        print("Invlaid Number please enter a number between 1-10")
        break
    if x == 0:
        print("Bye-bye")
        break

    fact = polynomial_function(x)
    print(fact)

###Question 2
#Animal – general class
 #Bird (child class of Animal)
 #Mammal (child class of Animal)
 #Fish (child class of Animal)
class Animal:
    def __init__(self,type,age,origin):
        self.type = type
        self.age = age
        self.origin= origin
    def show(self):
        print(f"That is a {self.type} It is {self.age} years old and is from {self.origin}")
class Bird(Animal):
    def moves(self):
        print("Fly")
class Mammal(Animal):
    def moves(self):
        print("Run")
class Fish(Animal):
    def moves(self):
        print("Swim")
Ostriches= Bird("Ostriches",2,"Africa")
Ostriches.show()
Ostriches.moves()
Blue_Jay= Bird("Blue Jay",3,"North America")
Blue_Jay.show()
Blue_Jay.moves()
Flamingos= Bird("Flamingos",4,"Asia")
Flamingos.show()
Flamingos.moves()


Panda= Mammal("Panda",2,"China")
Panda.show()
Panda.moves()
Tiger= Mammal("Tiger",4,"South Asia")
Tiger.show()
Tiger.moves()
Lion= Mammal("Lion",5,"Africa")
Lion.show()
Lion.moves()


Tuna= Fish("Tuna",3,"Pacific Ocean")
Tuna.show()
Tuna.moves()
Anchovies= Fish("Anchovies",3,"Black Sea")
Anchovies.show()
Anchovies.moves()
Baiji = Fish("Baiji",2,"Yangtze River")
Baiji.show()
Baiji.moves()
