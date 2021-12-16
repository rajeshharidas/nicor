FROM java:8



ADD /build/libs/nicor-0.0.1-SNAPSHOT.jar nicorapp.jar
RUN sh -c 'touch /nicorapp.jar'
ADD /build/libs/nicoranalysis.zip
 

EXPOSE 8097

ENTRYPOINT ["java","-Djava.security.egd=file:/dev/./urandom","-jar","/nicorapp.jar"]
