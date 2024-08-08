This is a Java implementation of a simple negotiation agent using the Genius Core framework. The agent generates random bids above a minimum target utility and adjusts its strategy based on the opponent's moves.

# Features


Generates random bids above a minimum target utility

Adjusts bidding strategy based on opponent's moves

Uses Genius Core framework for negotiation



# Bidding Strategy

The Bidding Strategy is responsible for generating bids that maximize the agent's utility. It uses a simple algorithm to calculate the next bid, taking into account the opponent's previous bids and the agent's own preferences.


- The bidding strategy generates a list of possible bids based on the agent's current knowledge.
- For each possible bid, the strategy calculates its utility using the Additive Utility Space provided by Genius Core.
- The strategy then selects the bid with the highest utility and proposes it to the opponent.


# Opponent Model

The Opponent Model is responsible for estimating the opponent's preferences and utilities based on their previous bids. It uses a simple algorithm to update the weights of each issue, taking into account the frequency of each option proposed by the opponent.


- The opponent model keeps track of the frequency of each option proposed by the opponent.
- For each issue, the model calculates its weight based on the frequency of each option.
- The model then updates the weights of each issue, taking into account the frequency of each option.

