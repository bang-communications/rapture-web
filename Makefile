all: package

compile:
	@mkdir -p bin
	@echo Compiling...
	@scalac -usejavacp -unchecked -deprecation -cp lib/log4j.jar:lib/aopalliance-1.0.jar:lib/bndlib.jar:lib/cloudfiles.jar:lib/cloudfiles-uk.jar:lib/cloudservers.jar:lib/cloudservers-uk.jar:lib/gson.jar:lib/guava.jar:lib/guice-assistedinject.jar:lib/guice.jar:lib/javax.inject.jar:lib/jclouds-blobstore.jar:lib/jclouds-compute.jar:lib/jclouds-core.jar:lib/jclouds-scriptbuilder.jar:lib/jersey-core-1.6.jar:lib/jsr305.jar:lib/openstack-common.jar:lib/osgi-compendium.jar:lib/osgi-core.jar:lib/postgres.jar:lib/rapture-io.jar:lib/rapture-orm.jar:lib/rapture-web.jar:lib/servlet-api.jar:lib/swift.jar -d bin src/*.scala

doc:
	@mkdir -p doc
	@echo Generating API documentation...
	@scaladoc -usejavacp -unchecked -deprecation -cp lib/log4j.jar:lib/aopalliance-1.0.jar:lib/bndlib.jar:lib/cloudfiles.jar:lib/cloudfiles-uk.jar:lib/cloudservers.jar:lib/cloudservers-uk.jar:lib/gson.jar:lib/guava.jar:lib/guice-assistedinject.jar:lib/guice.jar:lib/javax.inject.jar:lib/jclouds-blobstore.jar:lib/jclouds-compute.jar:lib/jclouds-core.jar:lib/jclouds-scriptbuilder.jar:lib/jersey-core-1.6.jar:lib/jsr305.jar:lib/openstack-common.jar:lib/osgi-compendium.jar:lib/osgi-core.jar:lib/postgres.jar:lib/rapture-io.jar:lib/rapture-orm.jar:lib/rapture-web.jar:lib/servlet-api.jar:lib/swift.jar -d doc src/*.scala

package: compile
	@echo Packaging rapture-web.jar
	@jar cmf etc/manifest rapture-web.jar -C bin rapture

clean:
	@rm -fr bin
	@rm -f rapture-web.jar
	@echo Cleaned bin and rapture-web.jar

.PHONY: test compile package clean
