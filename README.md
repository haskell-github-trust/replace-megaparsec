
# Parse Replace


HAQ (Hypothetically Asked Questions)

Q: Is it fast?

A: Meh. (benchmark comparison to sed).


attoparsec has match
http://hackage.haskell.org/package/attoparsec-0.13.2.2/docs/Data-Attoparsec-ByteString.html#v:match

attoparsec has a Monoid instance for Chunk
http://hackage.haskell.org/package/attoparsec-0.13.2.2/docs/Data-Attoparsec-Types.html#t:Chunk

but attoparsec does not work for String. so.

https://stackoverflow.com/questions/18957873/haskell-parenthesis-matching-for-find-and-replace

https://stackoverflow.com/questions/18338707/insert-a-character-into-parser-combinator-character-stream-in-haskell

https://pl-rants.net/posts/regexes-and-combinators/
