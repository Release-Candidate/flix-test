> [!WARNING]
> This repository has been moved to [Codeberg: codeberg.org/Release-Candidate/flix-test](https://codeberg.org/Release-Candidate/flix-test)

# Flix Test

Some random [Flix](https://flix.dev/) stuff.

* a type class for `Alternative` (and `GradedAlternative`) in [./src/Alternative.flix](./src/Alternative.flix) and [./src/GradedAlternative.flix](./src/GradedAlternative.flix)
* a small parser combinator module in [./src/FlixParse.flix](./src/FlixParse.flix)
* tests in [./test/](./test/)

## Usage

### Dependencies

* needs Java
* download the Flix compiler Jar from the [latest Release at GitHub](https://github.com/flix/flix/releases/latest)
* copy flix.jar into the project root

### Build targets

* display help: `java -jar flix.jar --help`
* initialize a new project: `java -jar flix.jar init`
* open a REPL of the project: `java -jar flix.jar repl`
* typecheck the project: `java -jar flix.jar check`
* run the program: `java -jar flix.jar run`
* run the tests: `java -jar flix.jar test`
* compile sources: `java -jar flix.jar build`
* generate a Jar: `java -jar flix.jar build-jar`

## License

Everything in this repository is licensed unter Apache 2.0, see file [./LICENSE.md](./LICENSE.md)
