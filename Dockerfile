FROM erlang:25-alpine as build

RUN mkdir /opt/arbeitsinspektor
WORKDIR /opt/arbeitsinspektor

COPY . /opt/arbeitsinspektor

RUN apk add --update git  \
    && rebar3 as prod release

FROM alpine:3.16 as application

RUN apk add --no-cache openssl libstdc++ ncurses-libs && \
    adduser -h /opt/arbeitsinspektor -u 1000 -s /bin/sh -D unprivileged

COPY --from=build --chown=unprivileged:unprivileged /opt/arbeitsinspektor/_build/prod/rel/arbeitsinspektor /opt/arbeitsinspektor

RUN ln -s /opt/arbeitsinspektor/bin/* /usr/local/bin/

USER 1000
WORKDIR /opt/arbeitsinspektor

CMD ["arbeitsinspektor", "foreground"]

