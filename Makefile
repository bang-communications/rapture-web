all: package

compile:
	@mkdir -p bin
	@echo Compiling...
	@scalac -usejavacp -unchecked -deprecation -cp lib/rapture-io.jar:lib/rapture-orm.jar:lib/log4j.jar:lib/servlet-api.jar:lib/osgi-core.jar:lib/osgi-compendium.jar -d bin src/*.scala

doc:
	@mkdir -p doc
	@echo Generating API documentation...
	@scaladoc -usejavacp -unchecked -deprecation -cp lib/rapture-io.jar:lib/rapture-orm.jar:lib/log4j.jar:lib/servlet-api.jar:lib/osgi-core.jar:lib/osgi-compendium.jar -d doc src/*.scala

package: compile
	@echo Packaging rapture-web.jar
	@jar cmf etc/manifest rapture-web.jar -C bin rapture

clean:
	@rm -fr bin
	@rm -f rapture-web.jar
	@echo Cleaned bin and rapture-web.jar

.PHONY: compile package clean
