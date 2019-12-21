FROM debian:8.6

# Install dependencies.
RUN apt-get update && \
  apt-get install --assume-yes curl gcc libgmp-dev libpq-dev make xz-utils zlib1g-dev

# Install Stack.
RUN curl --location https://www.stackage.org/stack/linux-x86_64-static > stack.tar.gz && \
  tar xf stack.tar.gz && \
  cp stack-*-linux-x86_64-static/stack /usr/local/bin/stack && \
  rm -f -r stack.tar.gz stack-*-linux-x86_64-static/stack && \
  stack --version

# Install GHC.
WORKDIR /project
COPY stack.yaml /project
COPY package.yaml /project
RUN stack setup && \
  stack exec -- ghc --version

# Install dependencies.
RUN stack build --only-dependencies

# Build project.
COPY . /project
RUN stack build --copy-bins --local-bin-path /usr/local/bin

# Run project.
ENV HOST 0.0.0.0
ENV PORT 80
EXPOSE 80
CMD /usr/local/bin/app
