# Blog Scraping + NLP Analysis

Experimenting with scraping (via the rvest package) and natural language processing analysis (via the NLP and openNLP packages). Used the Facebook API via the Rfacebook package to pull data on likes & shares for each post. Wrote this mainly for fun and a kooky obsession with Seth Godin's blog. Came in handy months later on a project for a client.

## Room for Improvement

For one, the scraping process would be a lot faster if I extracted multiple elements from an html page once, instead of extracting one element from the page, then later revisiting it in rvest and extracting another item. In the Analysis code, to reduce repetition I'd make a function or two for making and adding to data frames of NLP analysis vs. temporary data frames adding to larger ones. Would add clearer variable names.

I played around with some markov-babbling, and was considering throwing machine learning into the mix to see what a new post composed from random posts would look like, but that's a project for another time.
