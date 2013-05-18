Playground\Poker
==========

This project is an implementation of a hand simulator for Texas hold`em poker game. The way you communicate with it is by using a small DSL:

In all places where a card is expected, the format should be "rank""suit". So, "7H" stands for seven hearts.

In order to set your hand: just type it like this:

    00:00> ac qd

To add community cards: + [card]...

    00:00> + 7h 8d td

To calc probabilities of combinations: prob[?]:

    00:00> prob

To clear the state/environment:

    00:00> cl
    
To get some help:

    00:00> help
    
To set the pot: pot [=] num:

    00:00> pot 123
    
To add to the existing pot: pot + num:

    00:00> pot + 123
    
To calculate if it's wise to call a bet: call num_bet ["*" num_of_callers]

    00:00> call 123 * 2
