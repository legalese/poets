javapath = ../java/poetsio
javafxpath = ../javafx
thrift = @thrift
del = @rm -rf
mkdir = @mkdir -p

all:
	$(mkdir) $(javapath)
	echo "Making java thrift classes"
	$(thrift) -r -o $(javapath) --gen java poets.thrift

clean:
	$(del) $(javafxpath)/gen-java
	$(del) $(javapath)/gen-java
