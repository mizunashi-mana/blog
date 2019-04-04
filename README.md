# Mizunashi Mana's Blog

[![CircleCI](https://circleci.com/gh/mizunashi-mana/blog.svg?style=svg)](https://circleci.com/gh/mizunashi-mana/blog)

Powered by pelican.

https://mizunashi-mana.github.io/blog/

## Requirements

* pipenv: https://pipenv.readthedocs.io/en/latest/
* npm: https://www.npmjs.com/get-npm

## Installation

```bash
git clone https://github.com/mizunashi-mana/blog.git
cd blog
git submodule update --init
pipenv install
npm install
make publish
```

## Copyright

This works, except articles on the `content` directory, are under the [Apache 2.0 License](https://www.apache.org/licenses/LICENSE-2.0).
See the [LICENSE](LICENSE) file.

And, articles are under the [CC-BY-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/).
