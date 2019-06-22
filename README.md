# FTSE LARES


This repository provides building blocks for target formalisms  of the FTSE project wrt. algorithms, formalisms, tools and transformations. 

---

* **ftse.formalism** &ndash; Metamodels, Parsers and Serializers for 
  * **lares**: the *LARES* language 
  * **lares.flat**: a *LARES* hierarchy-less flat representation
  * **timenet**:  the *[Timenet](https://timenet.tu-ilmenau.de/)*  petri net solver language

* **ftse.transformations** &ndash; Transformations 
  * **spa**: *LARES* to *SPA* transformation 
  * **laresflat**: *LARES* to *Lares Flat* transformation
  * **additional**: *LARES* Instance Graph Serializer
  * **lares** 
  * **pn**
    
* **ftse.simulation** &ndash; Applied execution semantics ... 
  * laresflat
  * pn



--- 

## Prerequisites
Everything required for building should be specified in `build.sbt` using the [Simple Build Tool - SBT](https://www.scala-sbt.org/). It may also be a viable option to install Scala as well. 

## Building
To build and package the library 
```bash
sbt compile package
```

## Usage
Start e.g. java REPL
```bash
scala -cp target/scala-2.11/lares_2.11-1.0.0.jar
```

And import stuff ... 
```scala
scala> import ftse.formalism._
scala> ...
```
----
## Links: 
* [LARES website](http://lares.w3.rz.unibw-muenchen.de/about.html)
* [Design of Computer and Communication Systems Group @BUM](https://www.unibw.de/technische-informatik/mitarbeiter/professoren/siegle/forschung/entwurf-von-rechen-und-kommunikationssystemen)
* [PhD Thesis](https://athene-forschung.unibw.de/85049?query=A+Specification+Language+for+Reconfigurable+Dependable+System%2C+its+Formalization+and+Analysis+Environment&show_id=92070&id=85049)

