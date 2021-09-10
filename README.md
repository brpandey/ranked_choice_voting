Ranked Choice Vote
==================
![Logo1](https://raw.githubusercontent.com/brpandey/ranked_choice_vote/main/Vote1.jpg
![Logo2](https://raw.githubusercontent.com/brpandey/ranked_choice_vote/main/Vote2.jpg


> "The ballot is stronger than the bullet" 
> - Abraham Lincoln

The recent NYC mayorial race really piqued my interest here on ranked choice voting.
I decided to write a Haskell program to help build a little intuition here.

Given say 5+ candidates, voters have the ability to rank their top 5 MULTIPLE candidates in order of preference on the ballot.
If only the top three are specified or top two that is fine as well.  The unique thing is
that the lowest ranking candidate for each round (e.g. given 5 roounds for 5 ballot choices), that 
candidate's vote can MOVE to a different candidate.

For example let's say after the first round Candidate C ranks the lowest.  For everyone that voted
for Candidate C in round 1, we can give these ballots to whoever those people select for round 2.  Perhaps this
could be Candidate A or something like Candidate E.  Whatever the case, as long as that new recipient candidate
is still active and hasn't been eliminated the ballot changes hands and gets counted for the new candidate.

This happens until there are two candidates left, with the higher tallying candidate winning even if under 50%. 
If however, there is a clear outright vote leader in the first round with over 50% of the votes,
they become the winner preventing any further tallying.
 
Vote simulates this process by randomly generating vote ballots and ultimately determining the winner.


The [NYTimes](https://www.nytimes.com/interactive/2021/nyregion/ranked-choice-voting-nyc.html) has a great interactive article on how ranked choice voting works
The [NYC Civic Engagement Comission](https://www1.nyc.gov/site/civicengagement/voting/ranked-choice-voting.page) also has a good writeup

```haskell

> ghc Vote.hs
> ./Vote (# of ballots, default is 11)

BALLOT VOTES

Ballot {uid: 1, votes: [(1,A),(2,E),(3,C),(4,B),(5,D)], candidate history: [A]}
Ballot {uid: 2, votes: [(1,B),(2,C),(3,E),(4,D),(5,A)], candidate history: [B]}
Ballot {uid: 3, votes: [(1,E),(2,A),(3,C),(4,D)], candidate history: [E]}
Ballot {uid: 4, votes: [(1,D),(2,B),(3,A),(4,E),(5,C)], candidate history: [D]}
Ballot {uid: 5, votes: [(1,B),(2,C),(3,E),(4,A)], candidate history: [B]}
Ballot {uid: 6, votes: [(1,A),(2,D),(3,B),(4,C),(5,E)], candidate history: [A]}
Ballot {uid: 7, votes: [(1,B),(2,E),(3,C),(4,D),(5,A)], candidate history: [B]}
Ballot {uid: 8, votes: [(1,A),(2,E),(3,B),(4,D)], candidate history: [A]}
Ballot {uid: 9, votes: [(1,E),(2,A),(3,B),(4,C)], candidate history: [E]}
Ballot {uid: 10, votes: [(1,D),(2,B),(3,C),(4,E),(5,A)], candidate history: [D]}
Ballot {uid: 11, votes: [(1,C),(2,D),(3,B),(4,A),(5,E)], candidate history: [C]}


[B] WINNER!!! Candidate B has the most votes!
~Percentage -> 45.455 percent votes!
~Counts -> [(B,5),(D,4)] out of 11 total
~Note: (After all rounds counted, candidate with most votes wins EVEN if less than 50 percent)
~Winning Ballots: 
Ballot {uid: 1, votes: [(1,A),(2,E),(3,C),(4,B),(5,D)], candidate history: [A -> 3: From A to Pick 4=B]}
Ballot {uid: 9, votes: [(1,E),(2,A),(3,B),(4,C)], candidate history: [E -> 2: From E to Pick 3=B]}
Ballot {uid: 7, votes: [(1,B),(2,E),(3,C),(4,D),(5,A)], candidate history: [B]}
Ballot {uid: 5, votes: [(1,B),(2,C),(3,E),(4,A)], candidate history: [B]}
Ballot {uid: 2, votes: [(1,B),(2,C),(3,E),(4,D),(5,A)], candidate history: [B]}

~Discarded Ballots: 
Ballot {uid: 6, votes: [(1,A),(2,D),(3,B),(4,C),(5,E)], candidate history: [A -> 3: From A to Discard, Unable to transfer to now inactive candidate C]}
Ballot {uid: 3, votes: [(1,E),(2,A),(3,C),(4,D)], candidate history: [E -> 2: From E to Discard, Unable to transfer to now inactive candidate C]}
```
