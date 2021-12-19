Warm greetings to all zero of my readers. This blog has been fairly dead for a while now.
I've mostly been busy trying out a lot of things, and working on yet another
programming language this time (more on this later).

In the past several months I've made a simple observation about my learning methods: 
they're very ad-hoc and not disciplined. This often leaves me 'half-invested' in many different topics
and I end up having to choose one thing to learn, leaving everything else on a lonesome todo list.
When I find something interesting or fun, I end up scouring the web for books, papers, blog posts and all kinds of resources,  I get very invested into it and everything else that I should have been doing gets sidelined to an extent. I'm sure there is an analogy to be drawn between this behavior of mine and a poorly designed OS scheduler giving all it's CPU time to a single process while starving the others. But I won't make that bad pun today. Moving on...

For the past couple of months I've mostly been invested heavily in compilers, virtual machines, interpreters and optimization techniques. The primary reason for this is my project, [Vyse](https://github.com/srijan-paul/snap/) (again, more on this later). Save that, I've been able to find some good books to read.  Though my progress reading them has been very slow because I couldn't decide a proper order or schedule to read them in. Mentioned below is the list of books that I plan on finishing. I have a rough idea of how big each one is, so my plan is to cover half the list over what's left of this year.

- **[Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book.html)**: A part of canonical computer science and a rite of passage for CS undergrads. I found this book to be very resourceful, but I've only been able to read 2 out of the 5 chapters. It's a slow read, especially for non-lispers like me. For now, it remains on hold until I've caught up some other books and project related errands. That said, I really liked what I read so far and the exercises were a bit challenging. My work so far lives in this [github repo](https://github.com/srijan-paul/sicp).
- **[Practical Common Lisp](http://www.gigamonkeys.com/book/)**: This book is mostly to aid me in understanding what the fuss on LISP and it's derivatives is really all about. I figured out early on when reading SICP that it is more focused on shaping the reader's thought process. I wanted to learn more about *why* one would want to write production code with so many parenthesis and no types, and *why* CLOS is deemed so praiseworthy. My plans for this book are to read it side by side with SICP, to the fully savor the taste of LISP (and parens).
- **[Operating Systems: Three Easy Pieces](https://pages.cs.wisc.edu/~remzi/OSTEP/)**: I *really* like this book. I've read nearly half of it and it does an amazing job explaining advanced topics in little chunks over a chapter. The organization of chapters and the organization of chunks of paragraphs within those chapters in this book is really well thought out. I consider this book to be a preliminary requirement to join OSDev or start hacking on a hobby operating system. I plan on eventually reading the Dinosaur book at some point. Until then, this book scores top marks.
- **[3D Math Primer for Graphics and Game Development](https://gamemath.com/)**: I'm not the best at math, but I'm not totally terrible at it either. That said, It won't be wrong for me to admit that I find writing code easier than solving surface integrals. I've been looking for a time window to hone my mathematical skills and become a little more astute. I was looking forward to reading this book last year but was hesitant to buy it. Now that it is available online for free, I can a read little bit of time everyday reading it. Plus, it'll come in handy when I am going to eventually write a game framework for embedding Vyse (~~or continue work on one of the abandoned game projects~~).

These are the top few books in my to-read list, and they all happen to be tech related at the moment. There are some other books that I know I definitely want to read, but they're not as high priority as the ones mentioned above. [OS 0 to 1](https://tuhdo.github.io/os01/), [Engineering a compiler](https://www.amazon.com/Engineering-Compiler-Keith-Cooper/dp/012088478X), [Types and programming languages](https://www.cis.upenn.edu/~bcpierce/tapl/), [Computer graphics from scratch](https://gabrielgambetta.com/computer-graphics-from-scratch/) and [Compiling to assembly from scratch](https://keleshev.com/compiling-to-assembly-from-scratch/) to name a few.

You're right, that's enough books. So now I'll get a bit vague and mention names of topics I want to cover and become better at:

- **Computer networks** and **network programming** ([Beej's book ](https://beej.us/guide/bgnet/) is good resource for the latter)
- **Relational databases**. I've played quite a bit with MongoDB and Firebase. I don't dislike them by any means, just want to see what it's like on the the other side of the fence. I have my eyes on PostgreSQL.
- **Assembly**. I have a rough idea of what assembly programming is like. But I've only ever written toy 'Hello world' and 'add', 'mult' assembly programs. I want to explore this bit in a little more detail.
- **Color theory.** The benefits from this is twofold, the color schemes for my websites won't look hideous and my pixel art will finally look like it's been drawn by a single *ahem* artist.
- **Math.** Some advanced linear algebra, hopefully.
- **Type theory.** The type checkers that I've written so far work, but aren't as feature rich as I'd like them to be. The best I was able to do apart from general type checking is infer function return types and type check generics. And so this.
- **Advanced data structures.**  I've been doing a little bit of reading on the [cp-algorithms](https://cp-algorithms.com/) site everyday, the articles are fairly well written and contain links to related problems.
- **Concurrency in C++.** I'm not too good at writing multithreaded code, and have mainly written single threaded applications thus far. It doesn't feel great ignoring all the major advancements modern CPUs have made and staying put on one thread. So the plan is to properly learn multi-threading and look for more windows to write concurrent routines in my applications. 

And finally, I'm working two projects:

- [Woot](https://github.com/srijan-paul/woot). A twitter like social media app. It's primarily written in EJS and Javascript using NodeJS, Express and MongoDB.
- [Vyse](https://srijan-paul.github.io/vyse/): An interpreted, dynamically typed, scripting language for embedding in C++ applications like game engines and web servers.

## Wokay, so what's the title of this post about ?

Alright, I'll finally get to the point. The first paragraph had something to say about my ad-hoc ways of learning, and so I've decided to get in proper *shape* by keeping myself accountable. Accountable to who you ask? That's a good question.

Before getting into it, let me establish the goal one more time.

The goal is to **learn multiple things in a well structured manner but specialize in one/few of them**. A **T** shaped learning is what I hear it's called, though I don't quite see the correlation. 

## Digital Gardens... ?

I've been thinking about the aforementioned *structure* I want to follow for quite a long while now. Recently, I came across a [blog post](https://joelhooks.com/digital-garden) by Joel Hooks. It does a very good job of explaining what digital gardens are, so I highly recommend it. You may also find this very ambitiously named [repository](https://github.com/nikitavoloboev/knowledge) useful.

In short, a digital garden is an online showcase of a collection of ideas. It can be a form of writing, art, interactive data viz, music, or anything creative. In the sprit of Joel's blog and github repo, I want to do something similar. Grow an online *space* of some kind to showcase fields that I am interested in, the related work I've done and resources/interesting links. And I believe doing so will hone my skills in the process.

A garden in real life is very visual, that's the entire point of them. So for my digital garden, I want to do something similar and create a modular, visual way of keeping track of what I do. I haven't quite figured out *what* I'm going to exactly yet. Until I figure that out, I am going to keep track of my work in form of writing, collecting seeds and farming equipment and storing some of them in a git repo equivalent to Joel's "knowledge", and some in this blog for later use in the garden. You can consider this blog a prototypical digital garden until then :) 

This part was fairly short because I think Joel's blog does a great job of explaining this idea, so I don't want to repeat his points.

My next post is most likely going to be about the Vyse language and what I've carved out of it so far. See ya until then 👋.

