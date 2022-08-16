[Facebook already uses Haskell to filter spam](https://engineering.fb.com/2015/06/26/security/fighting-spam-with-haskell/)
on a much larger scale, with a much wider variety, and larger volume of input data.
So writing a spam fiter should be an easy task by comparison.

A [Naive Bayeisan classifier](https://en.wikipedia.org/wiki/Naive_Bayes_classifier)

## Preprocessing the dataset

### Loading the dataset

TODO

### Extracting tokens from the body of a message

For our usecase, a message can be considered to be a sequence of words separated by punctuation and whitespace.
Given any string - we want the all the substrings that are surrounded by "non-words" like whitespaces and punctuation marks.

First, let's define a helper that tells us whether a character is a delimiter or not:

```hs
isWordDelimiter :: Char -> Bool
isWordDelimiter x = isSpace x || x `elem` punctuations
  where
    punctuations = ",.!?;:'\"&-+-()[]=" 
```

Now, we want to iterate our way through a string, ignoring all sequences of delimiters
and adding all word sequences to our list of tokens.

```hs
tokenize :: String -> [String]
tokenize s = case dropWhile isWordDelimiter s of -- drop all prefix delimiters
  "" -> []
  remainingStr -> word : tokenize s'
    where
      -- fetch the next sequence of characters until we encouter another delimiter
      (word, s') = break isWordDelimiter remainingStr
```

### Removing stop words

Some words like "for", "in", "of", "when", "your", etc.
are common in all kinds of sentences and should not contribute towards classifying an email as Spam or Ham.
To avoid these common "stop words" from influencing our result,
we get rid of them altogether.

NLTK - a Python library for Natural Language Processing has a dictionary of stop words for the english language.
Thankfully, a kind GitHub user has listed all of them in [this gist](https://gist.github.com/sebleier/554280)

We can simply copy that into our Haskell module:

```hs
stopWords :: [String]
stopWords =
  [ "i",
    "me",
    "nor",
    -- ... goes on for a while
    "now"
  ]
```

Removing stop words is now fairly straightforward - filter a list of tokens,
keeping only those that do not belong to the `stopWords` list.

We also toss in another helper called `preprocessString` that tokenizes,
and then removes stop words.

```hs
removeStopWords :: [String] -> [String]
removeStopWords = filter (`notElem` stopWords)

preprocessString :: String -> [String]
preprocessString = removeStopWords . tokenize
```

## References

1. [Email message parsing in Haskell in 2018](https://gist.github.com/chrisdone/47a9e22672b54dedc87dec8b415e8127)
2. [Mime - working with mime types](https://hackage.haskell.org/package/mime)
3. [Smart classification in Haskell](http://www.randomhacks.net/2007/03/03/smart-classification-with-haskell/)
4. [NLTK's list of stop words](https://gist.github.com/sebleier/554280)

