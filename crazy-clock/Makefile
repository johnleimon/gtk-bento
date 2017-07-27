all: clock

clock::
	gnatmake -g -gnata -gnatybf -gnatwu $@ `gtkada-config`
clean::
	${RM} *.o *.ali animation b~*
