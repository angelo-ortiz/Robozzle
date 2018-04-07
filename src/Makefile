EXEC = robozzle
all:
	ocamlc -o $(EXEC) unix.cma str.cma graphics.cma adj.ml niveau.ml programme.ml vue.ml parsing.ml main.ml

run: all
	./$(EXEC)

clean:
	rm -f `find . -name "*.o"`
	rm -f `find . -name "*.a"`
	rm -f `find . -name "*.cm*"`
	rm -f `find . -name "*~"`
	rm -f $(EXEC)
