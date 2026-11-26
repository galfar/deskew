# Deskew command line tool, image published to ghcr.io/galfar/deskew by .github/workflows/docker.yml.
# Nothing is compiled here: the build context has the deskew binaries of the release archives,
# <arch>/deskew for each Docker architecture (amd64, arm64), and LICENSE.
# TARGETARCH is set by BuildKit (docker buildx); the legacy builder needs
# --build-arg TARGETARCH=amd64 (or arm64).
#
#   docker run --rm -v "$PWD:/data" ghcr.io/galfar/deskew -o out.png scan.png
#
# Files written to /data are owned by root unless the container runs with --user "$(id -u):$(id -g)".
FROM debian:trixie-slim

ARG TARGETARCH

# All codecs are built in; these are used when present: libjpeg-turbo is faster,
# libwebp makes WebP output possible, libtiff for TIFF features the built-in codec lacks
RUN apt-get update \
 && apt-get install -y --no-install-recommends libturbojpeg0 libtiff6 libwebp7 \
 && rm -rf /var/lib/apt/lists/*

COPY ${TARGETARCH}/deskew /usr/local/bin/deskew
COPY LICENSE /usr/share/doc/deskew/LICENSE

WORKDIR /data
ENTRYPOINT ["deskew"]
CMD ["-h"]
