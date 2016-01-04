# blog.haskellbr.com
Site Jekyll por trás de [blog.haskellbr.com](http://blog.haskellbr.com).

## Enviando seu post
* Crie um fork
* [Instale as dependências](#instalando-as-dependencias)
* [Rode o servidor](#comecando-o-servidor-com-livereload)
* Rode `jekyll post "Título"`
* Escreva algo legal
* Mande um PR

_NOTA:_ Vale a pena discutir o que vai escrever antes de começar. Veja se não
há ninguém online no [IRC](http://irc.lc/freenode/haskell-br) ou no
[Slack](http://slack.haskellbr.com/).

## Instalando as dependências
Tendo `ruby` e `bundler` instalados:
```
$ bundle install
```

## Começando o servidor
```
$ jekyll serve
```

## Começando o servidor com livereload
```
$ guard
# O servidor e o livereload vão começar
```

Instale a extensão [livereload](http://livereload.com/) no seu browser e visite
http://localhost:4000.

## Copyright & License
Todo o código e posts são distribuidos sob a [licensa MIT](LICENSE).
