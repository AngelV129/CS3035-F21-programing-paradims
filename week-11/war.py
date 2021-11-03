import random

values = {'Two': 2, 'Three': 3, 'Four': 4, 'Five': 5, 'Six': 6, 'Seven': 7, 'Eight': 8, 'Nine': 9, 'Ten': 10,
          'Jack': 11, 'Queen': 12, 'King': 13, 'Ace': 14}
suits = ['Hearts', 'Diamonds', 'Spades', 'Clubs']
ranks = ['Two', 'Three', "Four", "Five", "Six", 'Seven', 'Eight', 'Nine', 'Ten', 'Jack', 'Queen', 'King', 'Ace']


class Deck:
    def __init__(self):
        # Create list of card card object
        self.cards = [Card((rank, suit)) for suit in suits for rank in ranks]
        print(self.cards)

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
        self.cards = [Card((rank, suit)) for suit in suits for rank in ranks]


class Card:
    def __init__(self, rank_suit_tuple):
        self.rank, self.suit = rank_suit_tuple
        self.key = values.get(self.rank)

    def __lt__(self, other):
        return self.key < other.key

    def __gt__(self, other):
        return self.key > other.key

    def __eq__(self, other):
        return self.key == other.key

    def __repr__(self):
        return f'Rank: {self.rank}, Suit: {self.suit}, Key: {self.key}'

    def __str__(self):
        if self.key < 11:
            return f'{self.key} of {self.suit}'
        else:
            return f'{self.rank} of {self.suit}'


deck = Deck()

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
        p1Score += p1Card.key + p2Card.key
    elif p1Card < p2Card:
        print("Player 2 wins the hand.\n")
        p2Score += p1Card.key + p2Card.key
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