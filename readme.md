uPickle: a simple Scala JSON and Binary (MessagePack) serialization library

- [Documentation](http://com-lihaoyi.github.io/upickle/)


If you use uPickle and like it, you will probably enjoy the following book by the Author:

- [*Hands-on Scala Programming*](https://www.handsonscala.com/)

*Hands-on Scala* has uses uPickle extensively throughout the book, and has
the entirety of *Chapter 8: JSON and Binary Data Serialization* dedicated to 
uPickle. *Hands-on Scala* is a great way to level up your skills in Scala
in general and uPickle in particular.

For a hands-on introduction to this library, take a look at the following blog post:

- [How to work with JSON in Scala](http://www.lihaoyi.com/post/HowtoworkwithJSONinScala.html)

If you use uPickle and like it, please support it by donating to our Patreon:

- [https://www.patreon.com/lihaoyi](https://www.patreon.com/lihaoyi)

# Developer Docs

* Publishing is automatic on pushing tag to Github
* Updating docs is manual; 
    * `sbt upickleReadme/run`
    * `git checkout gh-pages`
    * `cp -R upickleReadme/target/scalatex/* .`
    * `git commit -am .`
    * `git push origin head`