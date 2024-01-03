In my recent effort to be better at Haskell,
I made a static site generator (which now powers this website).
This SSG (called Bark), provides a simple data format to represent the metadata for a webpage.
For a blog post, the metadata looks somewhat like this:

```javascript
{
  template: "post"
  tags: ["post" "personal"]
  title: "Foo bar"
  date: "2021-12-31"
  is_blog_post: "true"
}
```

The format is very JSON-like,
except its much easier to write and has 2 fewer data types than JSON (no numbers or booleans).
Once I was done writing [a simple parser](https://github.com/srijan-paul/bark/blob/main/app/Bark/FrontMatter.hs) for it, I ran it through a haskell channel on a discord server.
Some internet strangers rightly pointed out that my code had an atrocious amount of
packing and unpacking data from wrappers (like `Either ParserError ParseTree`).
I was also told that the right way to tackle this design problem in a functional language is to use
Monads, and that writing a parser is a great way to learn how monads work.

Shortly after, I had dug up the following resources to learn from and convert my parser into a magical monadic one:

- [Monads for functional programming](https://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf)
- [Monadic parser combinators](https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf)
- [A fistful of Monads (LYAH)](http://learnyouahaskell.com/a-fistful-of-monads)

What a monad is and how it can be used to structure functional programs is covered neatly in
the material above.
What I'd like to do instead, is document the changes I needed to make to my sorry little parser.

