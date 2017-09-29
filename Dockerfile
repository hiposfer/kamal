FROM clojure
RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app
COPY project.clj /usr/src/app/
RUN lein deps
COPY . /usr/src/app
RUN lein uberjar
EXPOSE 3000
CMD ["java", "-Xmx500m", "-Xss512k", "-jar", "target/kamal.jar"]