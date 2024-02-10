---
tags:
  - personal
  - miscellaneous
  - software
template: post
date: 2024-02-09
is_blog_post: true
title: But can you give up GitHub?
meta: If GitHub is so good, why isn't there a GitHub 2?
---
*"Those who forget history often inadvertently repeat it"*,
reads the opening line of [Give Up GitHub](https://sfconservancy.org/blog/2022/jun/30/give-up-github-launch/)'s launch page – an open manifesto by the software freedom conservancy that calls for leaving GitHub, and taking the next generation of FOSS elsewhere. 

The write-up is very strongly worded, and makes points that many will disagree with.
The mention of copilot, however, stands out.
Microsoft has been subject to several controversies since Copilot's launch.
But if GitHub is so good, it shouldn't matter, right? In a fair market everyone is allowed to compete, and the consumers get to choose whats best for them.

So, what's my problem?

## Microsoft is making money off your code 

GitHub used *every* public repository to train an AI model,
then released it as a for-profit product.
Their training data includes projects that have [copy-left licenses](https://en.wikipedia.org/wiki/Copyleft),
AKA: licenses that make it illegal for corporations to use your code in commercial software.
But... Copilot *is* commercial.
So what's Microsoft's excuse?
Their defense is that they only use your code for *training*,
and do not directly redistribute it, thereby eliding any license infringement.
This argument is complete nonsense, of course.

Generative AI models, by nature, regurgitate permutations of their training data.
[They do not create new information](https://twitter.com/Dr_Gingerballs/status/1755063988268798187).
Copilot can regurgitate your code, token-for-token, into someone else's project.
You might have seen this happen when copilot nimbly autocompletes a function body with comments and links that are directly taken from somebody else's repository.

This behavior is typical of Gen-AI tools, diffusion models, and LLMs.
ChatGPT has been found to [spit out paywalled NYT articles](https://www.nytimes.com/2023/12/27/business/media/new-york-times-open-ai-microsoft-lawsuit.html) word-for-word.
As a matter of fact, DALL-E, Midjourney, GPT,
and just about every generative AI model *does* store your data,
just not in a way that you can explain to a court.
They compress datasets and mangle them into weights,
which are then used to shuffle some random noise until it becomes legible.

Clearly, this is antithetical to FOSS and shareware licensing.
GitHub is (ab)using its leading position in the VCS hosting market to
curtail your ownership of programs that *you have written*.
## Own what you write

Proprietary LLMs scrape the entire internet – your blog, LinkedIn, Twitter, everything.

They break into your kitchen, steal your recipe, and give nothing back.
No references in generated answers.
No credits on their product page.
No ingress to your website.
Your traffic is stolen and monetized [[1](#backmatter)].

As explained in [this tweet](https://twitter.com/GergelyOrosz/status/1688829094249615360),
You can (try to) stop OpenAI from training on your code by adding this snippet to your website's `robots.txt` file [[2](#backmatter)]:

```
User-agent: GPTBot
Disallow: /
```

This only works against scrapers using the `GPTBot` user-agent, and is no harder to bypass than a "This website is for adults. Are you 18 or older?" pop-up. [[3](#backmatter)]

That's nearly all you can do to protect your essays so long as you keep them open.
But what about code?

## The cost of leaving GitHub

Unfortunately, quitting GitHub is harder than banging out `git remote set-url origin` into your shell.

Apart from repository hosting, GitHub has pull requests, issue tracking, [project management](https://github.com/features/issues), [a web IDE](https://github.dev), organization-wide [code search](https://github.com/features/code-search), [discussions](https://docs.github.com/en/discussions/collaborating-with-your-community-using-discussions/about-discussions), [site hosting](https://docs.github.com/en/pages/getting-started-with-github-pages/about-github-pages), [vulnerability detection](https://github.com/security), [dependency scanning](https://github.com/dependabot), and the hero product – GitHub actions [[4](#backmatter)].

If you move to a different host today, you need alternatives to all of these.
You may not need the entire cocktail of features, but you'll always want more than one,  and this is by design.

GitHub is deeply interwoven into the workflows of millions of developers.
Far too many are used to opening pull requests, rather than sending out email patches.
And thus, there is considerable friction in moving to a different host.

MS has created the Facebook of software development, a bazaar of projects and maintainers.
As Drew DeVault puts it:
>GitHub optimizes for the end-user and the drive-by contributor. sr.ht optimizes for the maintainers and core contributors instead. [[source](https://drewdevault.com/2018/06/05/Should-you-move-to-sr.ht.html)]

If you're used to GitHub, and like using it (like myself), leaving is rough.
## Can I give up GitHub?

Despite my moral disagreements with Microsoft and Copilot, I love GitHub.
I write all my code on it, and host this website on it.
I seldom find hitches that make me second-guess my choice in a VCS provider. There are some rough edges and downtimes, but the overall experience is refined.

With Microsoft's massive push to put Copilot on every platform,  it is evident that AI models are going to continue feeding off of everyone's GH repos.
Me leaving GH won't so much as make a scratch in Copilot's training data.
Moving to a different VCS provider is more of an ethical stand than anything else – against the upcoming oligopoly of AI merchants.
So, it all comes down to this:

> Is your craft important enough for you to bear the cost of moving to a lesser-known and less polished VCS Provider, to support shareware and free software? 

Depending on where this question falls on your compass, you're either unbothered by it, or already searching for alternatives.
As for myself, I'm torn, and can't quit GitHub right away.
- An organization that I'm a part of is exclusive to GitHub.
- My workplace uses GitHub.
- A GH profile is more easily recognized by potential employers.

So while I continue to use GitHub for work and existing personal projects,
I'm now actively looking for alternatives to host my future projects on.
## Alternatives

The two options that I'm evaluating are [codeberg](https://codeberg.org/) and [sourcehut](https://sr.ht/).
Each represents a different style of collaborative work.
While Codeberg follows GitHub's style of PR based workflows,
SourceHut builds on top of email patches.

I'm going to try both and settle on one to host larger personal projects, and eventually this website. If you're reading, then I encourage you to give other git hosts a chance, and see if makes sense for you to migrate [[5](#backmatter)].

I'm not particularly pleased with the way things are going, but I'm also unsure if I'm going to move just yet.
Perhaps Microsoft's next announcement will be the day I take this site off of GH pages, and [dare to connect a server to the internet](https://world.hey.com/dhh/dare-to-connect-a-server-to-the-internet-01d25a07).
## Backmatter

1.  While I'm heavily in favor of using LLMs for assisted learning, document summaries, and seeking answers; enabling this technology bears the dire cost of [stifling the human incentive behind sharing information](https://x.com/GergelyOrosz/status/1753141251631706474).
2. While my website does have a `robots.txt` now, it is still hosted on GitHub. No, the irony isn't lost on me.
3. The only other way that I know of is to hide your content behind captchas or paywalls. And at that point, a link to my website is a flaming rage bait.
4. And if that wasn't enough, GitHub has an an entire marketplace of apps that integrate with your repos. Even the company I work for is [an app on GitHub](https://github.com/marketplace/deepsource-io).
5. Often, it won't – and that's okay. A GH profile is touted a better resume by many employers. Moving several years of commits to a different host isn't easy for someone looking to land a job.
   
Also related: [Your words are wasted](https://www.hanselman.com/blog/your-words-are-wasted), Scott Hanselman.
