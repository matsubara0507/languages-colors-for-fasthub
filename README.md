# languages-colors-for-fasthub

Check diff language colors on [GitHub](https://github.com/github/linguist/blob/master/lib/linguist/languages.yml) and [FastHub](https://github.com/k0shk0sh/FastHub/blob/master/app/src/main/assets/colors.json).

This command is made in Haskell with stack.

```
$ stack exec -- check assets/languages.yml assets/colors.json
("Fantom",(type @= Just "programming" <: aliases @= Nothing <: ace_mode @= Just "text" <: codemirror_mode @= Nothing <: wrap @= Nothing <: extensions @= Just [".fan"] <: interpreters @= Nothing <: searchable @= Nothing <: language_id @= 110 <: color @= Just "#14253c" <: tm_scope @= Just "source.fan" <: group @= Nothing <: nil,color @= Just "#dbded5" <: url @= "https://github.com/trending?l=Fantom" <: nil))
("Haskell",(type @= Just "programming" <: aliases @= Nothing <: ace_mode @= Just "haskell" <: codemirror_mode @= Just "haskell" <: wrap @= Nothing <: extensions @= Just [".hs",".hsc"] <: interpreters @= Just ["runhaskell"] <: searchable @= Nothing <: language_id @= 157 <: color @= Just "#5e5086" <: tm_scope @= Nothing <: group @= Nothing <: nil,color @= Just "#29b544" <: url @= "https://github.com/trending?l=Haskell" <: nil))
```
