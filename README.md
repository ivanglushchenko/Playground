Playground\Poker
==========

This project is an implementation of a hand simulator for Texas hold`em poker game

The way you communicate with it is by using a small DSL:
1) In all places where a card is expected, the format should be "rank""suit". So, "7H" stands for seven hearts.
2) In order to set your hand: just type it like this:

00:00> ac qd

3) To add community cards: + [card]...

00:00> + 7h 8d td

4) To calc probabilities of combinations:

00:00> prob

