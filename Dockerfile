# first build stage -- will be discarded
FROM clojure AS builder
WORKDIR /app
COPY ./project.clj ./
RUN lein deps
COPY . .
RUN lein uberjar

# second stage -- executable
FROM openjdk:alpine
WORKDIR /usr/src/app
COPY --from=builder /app/target/kamal.jar ./target/kamal.jar
COPY --from=builder /app/resources/ ./resources
EXPOSE 3000
CMD ["java", "-Xmx500m", "-Xss512k", "-jar", "target/kamal.jar"]
