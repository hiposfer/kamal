# first build stage -- will be discarded
FROM clojure AS builder
WORKDIR /app
COPY ./project.clj ./
COPY . .
RUN lein with-profile uberjar uberjar

# second stage -- executable
FROM openjdk:alpine
WORKDIR /usr/src/app
COPY --from=builder /app/target/kamal.jar ./target/kamal.jar
COPY --from=builder /app/resources/ ./resources
EXPOSE 3000
CMD ["java", "-Xmx500m", "-XX:+UseG1GC", "-XX:+UseStringDeduplication", "-Dclojure.compiler.direct-linking=true", "-Dclojure.compiler.elide-meta=\"[:doc :file :line :added]\"" , "-jar", "target/kamal.jar"]
