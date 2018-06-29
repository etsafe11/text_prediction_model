Johns Hopkins University Coursera Data Science Specialization & SwiftKey
n-gram Text Prediction Model

Executive Summary
========================================================
* We have built an easy-to-use and highly accurate text prediction algorithm.
* Beginning with just raw text files consisting of several million blog posts, tweets and
news articles, we have cleaned and tokenized the text and built an n-gram text prediction
model.
* To use the product, the user simply navigates Shiny app on the web, enters any amount 
of text and then almost instantaneously receives a prediction for the next word.  Simple!
* [Click here](https://github.com/etsafe11/text_prediction_model) for 
the GitHub repo containing a README and full R scripts for our Shiny app
* Our Shiny app is deployed here:  [this link](https://etsafe11.shinyapps.io/n-gram_text_prediction_model/)

Description of the Algorithm 
========================================================
* We first obtained a random sample of the English language from a web crawling service which 
sampled several million blog posts, tweets and news articles.  We merged these text files,
took a random sample, converted it to a `corpus` object and created the tidy input dataset
for our model.
* We did this by finding the most frequently-ocurring groups of words ("n-grams"). For example
"I really love football" is a 4-gram and "me too" is a 2-gram.
* Given a particular n-gram entered by the user, our algorithm first tries to find the 
n-gram in our input dataset, and if it is found, the user receives back the subsequent word 
that most-frequently follows that particular n-gram.
* If the user's n-gram is not found, then a list of so-called "skip-grams" is created from the 
n-gram, and the algorithm searches sequentially for each of these skip-grams in the dataset. As 
soon as a match is found, the associated predicted word is returned, just as described above.
* In our above example of "I really love football", skip-grams would include "I love football",
"I really football", "really love football" and "I really love".
* Our algorithm eliminates and skip-grams which do not contain the final word entered by
the user. In the above example, the n-gram "I really love" would thus not be considered as a 
possible match in our dataset. This improves model accuracy because the final word in the 
input is often very closely related to the prediction word; eliminating it hurts accuracy.


Model Accuracy
========================================================
* We tested model accuracy by partitioning the original corpus into training and
testing corpora. We then sampled 250,000 random n-grams from the testing corpus 
and and evaluated our model over this set.
* Our model shows accuracy of 5.87%. In other words, given a random input of text 
(an n-gram), our model correctly predicts the subsequent word approximately 5.87% of 
the time.
* For context, The Second Edition of the 20-volume Oxford English Dictionary contains 
approximately 171,000 words in current use.
* In other words, our model's 5.87% accuracy is approximately 10,000 times better 
than a random guess.  That is a terrific result.
* Please contact the author, Eric Thompson (etsafe11@gmail.com), with any inquiries.
