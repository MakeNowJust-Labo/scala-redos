package codes.quine.labo.redos
package regexp

class RegExpException(message: String) extends Exception(message)

class UnsupportedRegExpException(message: String) extends RegExpException(message)

class InvalidRegExpException(message: String) extends RegExpException(message)
