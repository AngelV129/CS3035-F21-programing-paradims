import random

values = {2: 2, 3: 3, 4: 4, 5: 5, 6: 6, 7: 7, 8: 8, 9: 9, 10: 10,
          'Jack': 11, 'Queen': 12, 'King': 13, 'Ace': 14}
suits = ['Hearts', 'Diamonds', 'Spades', 'Clubs']
ranks = [2, 3, 4, 5, 6, 7, 8, 9, 10, 'Jack', 'Queen', 'King', 'Ace']


class Deck:
    def __init__(self):
        # Create list of card card object
        self.cards = [Card((rank, suit)) for suit in suits for rank in ranks]

    # shuffle cards member
    def shuffle_deck(self):
        random.shuffle(self.cards)

    # Draw card from deck (returns a card)
    def draw(self):
        return self.cards.pop(0)

    # Return boolean if deck is empty
    def isEmpty(self):
        return len(self.cards) == 0

    # resets the deck (creates a new list of cards and shuffles them)
    def reset(self):
        self.__init__()
        Card.unique_id = 0
        self.shuffle_deck()


class Card:
    unique_id = 0

    def __init__(self, rank_suit_tuple):
        self.rank, self.suit = rank_suit_tuple
        self.value = values.get(self.rank)
        Card.unique_id += 1
        self.key = Card.unique_id

    def __lt__(self, other):
        return self.value < other.value

    def __gt__(self, other):
        return self.value > other.value

    def __eq__(self, other):
        return self.value == other.value

    def __repr__(self):
        return f'Rank: {self.rank}, Suit: {self.suit}, Key: {self.key}, Value: {self.value}'

    def __str__(self):
        # if self.key < 11:
        #     return f'{self.rank} of {self.suit}'
        # else:
        return f'{self.rank} of {self.suit}'


deck = Deck()
print("\n\nLet's Play War!\n")

p1Score = 0
p2Score = 0

print("Shuffling Deck.")
deck.shuffle_deck()

game_round = 1
while game_round <= 5:
    hit = input("\nType 1 to hit!")

    p1Card = deck.draw()
    print("\nPlayer 1 has ", end='')
    print(p1Card)
    p2Card = deck.draw()
    print("\nPlayer 2 has ", end='')
    print(p2Card)
    print()

    if p1Card > p2Card:
        print("Player 1 wins the hand.\n")
        p1Score += p1Card.value + p2Card.value
    elif p1Card < p2Card:
        print("Player 2 wins the hand.\n")
        p2Score += p1Card.value + p2Card.value
    else:
        print("Tie hand.\n")

    print("Player 1 has " + str(p1Score) + " points.")
    print("Player 2 has " + str(p2Score) + " points.\n")

    game_round += 1

if p1Score > p2Score:
    print("Player 1 wins the game!\n")
elif p1Score < p2Score:
    print("Player 2 wins the game!\n")
else:
    print("The game is a tie!\n")
quit()
