###
# Currently using version 0.83 of scala-bison is working with
#
# bison 2.6.2
# scala-2.9.2 
# jflex-1.5.0-SNAPSHOT
#
# The version supporting scala for jflex was compiled from https://github.com/moy/JFlex (branch V1.5)
# 
# Scala-bison version 0.83 does not compile with 2.10.x because uses a method
# who change the visibility from scala 2.9.x to 2.10.x
#
###
SCALA_HOME=~/src/scala-2.9.3/
SCALA=${SCALA_HOME}/bin/scala
SCALA-BISON=lib/scala-bison.jar
JFLEX=lib/jflex-1.5.0-SNAPSHOT.jar
BISON=bison -v
JAVA=java
RM=rm -f

OBJS=TigerParser.scala TigerTokens.scala TigerScanner.scala

all: $(OBJS)

TigerScanner.scala : Tiger.lex
	$(JAVA) -jar ${JFLEX} --scala Tiger.lex

TigerParser.scala TigerTokens.scala : Tiger.y
	$(BISON) Tiger.y
	$(RM) Tiger.tab.c
	$(SCALA) -cp ${SCALA-BISON} -howtorun:object edu.uwm.cs.scalabison.RunGenerator -v Tiger.y
	$(RM) Tiger.output

clean :
	rm -f *.class *Parser.scala *Tokens.scala *Scanner.scala *~ *.output *.lcoutput *.tab.c
