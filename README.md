# Mizunashi Mana's Blog

[![CI Build](https://github.com/mizunashi-mana/blog/actions/workflows/build-for-ci.yml/badge.svg)](https://github.com/mizunashi-mana/blog/actions/workflows/build-for-ci.yml)

Powered by pelican.

https://mizunashi-mana.github.io/blog/

## Requirements

* nix-direnv: https://github.com/nix-community/nix-direnv

## Installation

```bash
git clone --recurse-submodules https://github.com/mizunashi-mana/blog.git
cd blog
# install requirements automatically by nix-direnv
make publish
```

## Copyright

This works, except articles on the `content` directory, are under the [Apache 2.0 License](https://www.apache.org/licenses/LICENSE-2.0).
See the [LICENSE](LICENSE) file.

And, articles are under the [CC-BY-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/).
