FROM ubuntu:22.04

# If we don't set locale this way and try to print emoji to stdout, the server will crash.
ENV LANG=C.UTF-8

# Note: 
# ca-certificates solves an issue with some SSL certificates 
# libtinfo5 makes the binary ever run
RUN apt-get update && apt-get install -y ca-certificates libtinfo5 && update-ca-certificates

COPY --from=bin_path_context server /app/server

WORKDIR /app

EXPOSE 8000

CMD ["/app/server"]