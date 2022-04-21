import random

# user defined function to print the board
# it accepts one argument - the list of values (numbers, or x or o)
# it doesn't return anything
def print_board(p):
    counter = 0
    for row in range(3):
        for col in range(3):
            print(p[counter], end=' ')
            counter += 1
        print('')

# a function to check if anyone won the game
# it checks if there's a winner using the input symbol which is 'x' or 'o'
# it returns a boolean value that indicates whether the use with that symbol won the game
def isWinner(symbol):
    player_wins = False
    if positions[0] == positions[4] == positions[8] == symbol:
        player_wins = True
    if positions[2] == positions[4] == positions[6] == symbol:
        player_wins = True
    if positions[0] == positions[1] == positions[2] == symbol:
        player_wins = True
    if positions[3] == positions[4] == positions[5] == symbol:
        player_wins = True
    if positions[6] == positions[7] == positions[8] == symbol:
        player_wins = True
    if positions[0] == positions[3] == positions[6] == symbol:
        player_wins = True
    if positions[1] == positions[4] == positions[7] == symbol:
        player_wins = True
    if positions[2] == positions[5] == positions[8] == symbol:
        player_wins = True

    # if none of the if statements above was true then no one won
    # the value of player_wins will be False
    # otherwise it will be True
    return player_wins

# the list of values on the board
# initially it's all numbers
positions = ['0', '1', '2', '3', '4', '5', '6', '7', '8']

# print the initial state of the board using the user defined function
print_board(positions)

# loop until something breaks the loop
# the loop is broken if someone wins or
# if the user enters a value outside of the 0-8 range
while True:

    # get input from the user
    location = input('Where do you want to put an x?')
    # input from the console is always a string. We need to convert it to a number
    location = int(location)

    # check if the input was a valid position. If not then the user doesn't want
    # to play anymore. Exit the loop
    if location < 0 or location > 8:
        break

    # if the input was valid then update the board by placing 'x' at the
    # selected location
    positions[location] = 'x'

    # print the new state of the board
    print_board(positions)

    # check if the user won
    user_wins = isWinner('x')

    # if they won congratulate them and exit the game
    if user_wins == True:
        print("Congratulations, you won!")
        break

    # the computer selects a random number between 0 and 8
    comp_location = random.randint(0, 8)

    # check that the selected position is available
    # if not then keep selecting until he hits a valid spot
    while True:
        if positions[comp_location] in ('o', 'x'):
            comp_location = random.randint(0, 8)
        else:
            break

    # update the board to reflect the computer's move
    positions[comp_location] = 'o'

    # check if the computer won
    comp_wins = isWinner('o')

    # if he won then tell the user they lost and exit the game
    if comp_wins == True:
        print("You lose!")
        break

    # this is just a separator to make the console output more readable
    print('--------------------------------')

    # print the new version of the board (after the computer selection)
    # you may want to move that before the check if he won
    print_board(positions)

