# Blog Scraping

Experimenting with scraping (via the rvest package) and natural language processing analysis (via the NLP and openNLP packages). Used the Facebook API via the Rfacebook package to pull data on likes & shares for each post. Wrote this mainly for fun and a kooky obsession with Seth Godin's blog, but what I learned writing it came in handy months later on a project for a client.

## Room for Improvement

For one, the scraping process would be a lot faster if I extracted multiple elements from an html page once, instead of extracting one element from the page, then later revisiting it in rvest and extracting another item. In the analysis code, I'd focus on the DRY principle, and have made a function or two to cut down on the code for making data frames of NLP analysis. 