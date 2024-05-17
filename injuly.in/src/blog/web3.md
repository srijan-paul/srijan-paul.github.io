---
template: "post"
tags: ["web3", "blockchain", "technology" ]
title: "My gripes with NFTs and Web3"
date: "2022-01-19"
meta: "My probelm with the blockchain ecosystem."
is_blog_post: true
---

During my short stint as a blockchain developer, I've learned a lot and have met amazing people.
While I acknowledge the skill and reverence of some developers in this domain,
I still stand by my opinion that blockchain technology is not the panacea it's made out to be.
Rather, it's a solution in search of a problem.

Developers of NFT projects aren't blind to the technical limitations of blockchain.
Yet they choose to write them off as "tradeoffs" for *web3*, or worse, turn a blind eye to them.
Worse are the social problems - ones that can't be tackled by any number of token standards or consensus algorithms.

## The majority of crypto is shady, even for artists.

Show me one successful crypto project that has served a real purpose,
and I'll show you ten that were outed for being scams.

NFTs claim to be a revolution for artists and creators.
But what happens when someone makes an NFT out of stolen art?
Owing to a blockchain's immutable nature,
the thief now owns the art forever, even if they're found out.

Crypto evangelists will propose that artists mint an NFT out of their art before anyone else can.
I don't have to explain why this is a terrible idea.
Why should an artist **pay** to register an NFT on Flow, Ethereum, Solana, Monero, and countless other chains under the sun?

## NFT marketplaces are centralised and controlled.

The willingness to be free from institutional regulation and government censorship is at the fulcrum of the web3 movement.
Being able to trade assets without fearing a regulatory ban is promising if you're in certain trades.
However, this promise is immediately undermined by a simple fact –  the marketplaces themselves are centralised and controlled.

NFT assets **can** be removed from [objkt](https://objkt.com/) and [opensea](https://opensea.io/), you **can** be banned from [coinbase](https://www.coinbase.com)
[Opensea even admits it](https://support.opensea.io/hc/en-us/articles/1500010625362-Why-are-my-items-and-collections-delisted-) themselves!
If you violate any of their vague "rules",
your NFT will be delisted from a supposedly "decentralised" and "uncensorable" marketplace:

- infringe on protected intellectual property,
- promote suicide or self-harm,
- incite hate or violence against others,
- degrade or dox another individual,
- otherwise violate our [terms of service](https://opensea.io/tos). 

But what constitutes "violence or hate against others"?
Whatever their definition is, one thing is abundantly clear – 
the people moderating this website control our access to it.
"Moderating" a decentralised and censor-proof system is an oxymoron.

### NFTs do not have any value

In stark contrast with their price tags, the actual value represented by an NFT is surprisingly low.
Imagine, for a moment, that you bought this rock NFT:

![rock-nft](https://pbs.twimg.com/profile_images/1423533825028616195/-iFC12qC_200x200.jpg)

The image is yours, and you claim all rights to its usage and redistribution.
Hitherto, there shall only be boulder and one owner.

Right? <br/>
Wrong. <br/>

The artist still owns the image, and they're entitled to sell it to others,
or even make yet another NFT out of it on a different blockchain and sell it there.

When an asset is digital, and an image at that, "ownership" comes with no priviledges.
If you can use it as your twitter avatar, I can too.

Whatever you can do it with it, I can.

### Your 'get-rich-quick' chances are miniscule.

> The value of an NFT is what you believe it to be.

Crypto evangelists would have you believe this too, except they'd word it very differently.
But you can't get the world to agree on this absurd system 
where nearly identical 32x32 pixel jpegs are supposed to encapsulate an "idea" worth thousands of dollars.

To most, it just won't make sense.
Why should this clump of pixels cost 1M$?

<img src="https://cryptopotato.com/wp-content/uploads/2021/03/cryptopunk.jpg" width="200" height="200"/>

Why should we collectively enforce this belief? <br/>

Some crypto bros will claim it can make you rich(https://cryptopotato.com/this-cryptopunk-nft-was-bought-for-15k-in-2018-now-sold-for-8-million/)",
and there is a nugget of truth to that, but it's a very twisted statement that conceals more than it shows.
We've to realize that this value doesn't come from nowhere.
For you to have gained in this flaky landscape, several others had to lose.
And if general probability theory is anything to go by, you're more likely to find yourself in the losing lot.

MLM scams and pyramid schemes play a surprisingly similar game,

## Engineering issues

### NFTs can be altered

Contrary to popular belief, the actual data represented by NFTs aren't stored on a blockchain.
i.e - the million dollar ape JPEG someone spent their life savings on is physically located somewhere *outside* Ethereum.
They're stored on a separate database which could be centralised (but generally isn't, like IPFS).
The blockchain itself only stores a URI to the resource in question (say `ipfs://foo/bar/orangutan.jpg`).

Look at Ethereum's [EIP-721](https://eips.ethereum.org/EIPS/eip-721) token standard.
Once you've made your way through the document, you'll realise there is **nothing** stopping a server
from serving different assets for the same URI depending on the client requesting it.
See [this NFT](https://moxie.org/2022/01/07/web3-first-impressions.html#making-an-nft) by Moxie Marlinspike as an example.

### Internet on the blockchain? 

On platforms like LinkedIn and Twitter, I've seen sordid claims of a "completely decentralized internet" based on blockchain.
That there will be no institutional control, and the governments' will be forced to relinquish their control of social media. 
The claim goes that this will be by virtue of a blockchain driven internet that runs on gas fees, and not ads.

It just isn't possible.
An ad-driven web, horrible as it is, is the one model that is known to work. 
People don't mind their data harvested if it means they can chat with their friends for free.
But they *will* mind paying for every message, every post, every like, and every tangential interaction they have on this web3 tinternet.

However, lets leave the human factors aside for a moment.
Let's imagine a world where humans willingly pay to post (and like) instagram selfies.
Even still, blockchain is incapable of supporting such an infrastructure.

Blockchains are *slow*.
They're simply inefficient when put up against traditional databases with decades behind them. 
Even worse is traditional scaling and optimization techniques do not apply to a blockchain.
The optimizations that do exist ([indexers](https://wiki.tezosagora.org/build/blockchain-indexers)) are centralized, and beat the whole point of web3.

A blockchain is a large append-only merkle tree that needs the computing power of a suburb to confirm a group of transactions,
and even then it can take minutes depending on the implementation.
For highly critical transactions,
you've to wait for 5-6 rounds of "confirmation" before gaining reasonable confidence for any operation.

Anyone who has actually studied this subject in appropriate detail would tell you it is impossible for such inefficient technology to withhold traffic on sites like reddit, discord, twitter, facebook, 4chan, youtube etc.

The more energy efficient consensus algorithms, like "Proof of Stake", have a yet another flaw.
They favor miners (called [bakers](https://wiki.tezosagora.org/learn/baking) on the Tezos chain) who own more coin.
A classic "rich get richer" scheme.

