# Bazel Configuration

Bazel is a build system based on internal Google build system called Blaze.
Homepage: [bazel.build](https://bazel.build/)

Bazel configuration files are documented on:
[docs.bazel.build/versions/master/guide.html#bazelrc
](https://docs.bazel.build/versions/master/guide.html#bazelrc)

We are using Dhall to as a templating system to be able to use environment
variables.

List of relevant files:

*   [bazelrc.dhall](./bazelrc.dhall) – Template of `~/.bazelrc` file.
*   `local.bazelrc` – File that is imported by
    [bazelrc.dhall](./bazelrc.dhall), but not part of this repository.  It is
    assumed to be machine specific.  Common content of this file is:

    ```
    build --disk_cache=/path/to/build/cache
    ```
