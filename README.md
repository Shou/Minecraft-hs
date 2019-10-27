
A very rudimentary collection of Minecraft city structures written in Haskell, based on [this blog post][1].

There is no Cabal or Stack project here, but you run GHCi with `nix-shell` to ensure you have the dependencies:

```bash
nix-shell -p ghc.withPackages(ps: with ps; [random lens monad-loops]) --run ghci Minecraft.lhs
```

[1]: http://www.timphilipwilliams.com/posts/2019-07-25-minecraft.html
