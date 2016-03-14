Since I'm a solo team, it's my code. It is worth mentioning, however, that my dad helped a lot with general
Haskell knowledge, as well as help with implementations of certain things. Basically, he was a Haskell TA.
He also did some IO stuff, like modifying the Java file and showing me how to make the Makefile.

Improvements:
I based my heuristic function on https://kartikkukreja.wordpress.com/2013/03/30/heuristic-function-for-reversiothello/;
in particular, in the sub-heuristics I implemented, I used the weightings given in the c++ file. This was *very*
helpful--it turns out that corners are overwhelmingly more important than anything else in the game. 
(Against BetterPlayer, Sam usually keeps only a few pieces throughout the beginning and middle of the
game, while making the opponent move in the spaces next to the corners--then takes almost the entire
board in the last few moves.) Also helpful was the mobility heuristic--Sam often forces its opponent to pass
multiple times in a row.

But a heuristic function alone isn't enough (even though with just the heuristic, it *did* beat BetterPlayer a
couple of times...). So I implemented minimax. After some testing, I settled on 4 moves deep as having the
best balance of time and effectiveness. (Unfortunately and ironically, I have insufficient time remaining to
implement a way to handle time remaining.)

I had planned to implement alpha-beta pruning, use a database of opening moves, and to switch strategies
depending on whether it was the beginning, middle, or end of the game, but I ultimately didn't have enough
time for these improvements.