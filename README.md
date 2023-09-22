# E-Tourism Recommendation System

## Overview

The E-Tourism Recommendation System is designed to personalize travel recommendations for users based on their preferences. It utilizes data about available travel offers, including accommodations, transportation options, and customer preferences, to make tailored recommendations in the tourism industry.

## Project Description

In today's digital age, recommendation systems have become common in various industries, such as music streaming apps and online shopping platforms. This project brings the benefits of recommendation technology to the tourism industry, catering to the diverse preferences and needs of travelers.

## How It Works

Travel agencies provide information about available travel offers and customer preferences. The system takes customer preferences into account when recommending offers. Customers specify their preferences, including activities, accommodation types, budget constraints, transportation preferences, and travel periods.

The system aims to satisfy as many of these preferences as possible and recommends travel offers accordingly. It can also recommend offers for groups of travelers, considering individual preferences and offer capacity.

## Facts

The system relies on the following facts:

- **offerMean/2**: Specifies the means of transportation for each travel offer.

- **offerAccommodation/2**: Specifies the type of accommodation for each travel offer.

- **customerPreferredMean/3**: Customers rate their transportation preferences, including the means of transportation and a relevance rating.

- **customerPreferredAccommodation/3**: Customers rate their accommodation preferences along with a relevance rating.

- **customerPreferredActivity/3**: Customers rate their activity preferences with a relevance rating.

## Predicates

The system includes several predicates to ensure its functionality:

- **possibleSubset/2**: Generates all permutations of every subset of a given list.

- **choosePreferences/2**: Selects a set of preferences from a customer's input preference list based on the customer's requirements, which can include destination, budget, transportation means, accommodation type, activities, and travel period.

- **preferenceSatisfaction/4**: Calculates how much a customer is satisfied by a given offer based on the chosen preferences.

- **overlapPeriod/2**: Checks if two specified periods overlap.

- **getOffer/2**: Retrieves an offer that matches a list of chosen preferences, considering overlaps in travel periods.

- **recommendOfferForCustomer/3**: Chooses a subset of preferences that can be satisfied by a given offer for a specific customer.

- **recommendOffer/4**: Recommends an offer for a group of customers, considering their individual preferences and the maximum number of guests an offer can accommodate.

## Getting Started

1. Clone the repository to your local machine.

2. Install a Prolog interpreter (e.g., SWI-Prolog).

3. Load the project files into the Prolog interpreter.

4. Use the implemented predicates to recommend travel offers based on customer preferences.
