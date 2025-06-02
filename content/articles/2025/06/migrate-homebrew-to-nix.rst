Homebrew から Nix に移行する
=================================

:tags: Homebrew, Nix, macOS, Fish
:category: 環境構築

`Homebrew <https://brew.sh/>`_ は macOS における開発者の標準のパッケージマネージャだ。 `Homebrew`_ を使えば、殆どのパッケージの最新バージョンを ``brew install`` コマンドだけで簡単にインストールできる。一方で、バージョン固定が難しく、全て最新バージョンを追い続けるか、 `Homebrew`_ がサポートしているバージョンでないと使えない。開発環境では、プロジェクトによって異なるバージョンのツールを併用したい場面が多く、環境も分けたい場合が多い。また、パッケージの依存関係までバージョンを固定したい場合もある。そのような場面では、 `Homebrew`_ は不向きだ。

`Nix <https://nixos.org/nix/>`_ は同じく、パッケージマネージャで、パッケージのバージョンを固定したり、環境を分けたりすることができる。特に、 `Nix`_ は、パッケージの依存関係を全て固定することができるため、開発環境を簡単に再現することができる。加えて、 `Nix Darwin <https://nix-darwin.github.io/nix-darwin/manual/index.html>`_ や `Home Manager <https://nix-community.github.io/home-manager/>`_、 `nix-direnv <https://github.com/nix-community/nix-direnv>`_ といったツールを使うことで、システム設定や設定ファイルまで加えて、より柔軟にパッケージ管理を行うことができる。また、 `Nix`_ が標準で提供するパッケージ群の数は、 `他より多く <https://repology.org/repositories/statistics/total>`_、これも一つ魅力と言えるだろう。

最近、 `Homebrew`_ から `Nix`_ に完全移行し、わりかし快適になったので、今回は `Nix`_ の機能の紹介と、 `Homebrew`_ からの移行方法を紹介する。

Nix の機能とエコシステム
------------------------------

`Nix`_ は、再現可能で宣言的なパッケージ管理を可能とするツールであり、エコシステムの総称ともなっている。 `Nix`_ では、

* パッケージをインストールする CLI
* それを活用しやすくするためのシェル
* Nix とのやり取りを行うための Nix 言語
* Nix 言語ライブラリ自体の依存管理機能 Flake

などの機能が提供されている。また、Nix 言語ライブラリにもユニークなパッケージがいくつかあり

`Home Manager`_
    ユーザの設定を Nix で管理するためのライブラリ

`Nix Darwin`_
    macOS の設定を Nix で管理するためのライブラリ。 `Home Manager`_ との連携機能もある

`nix-direnv`_
    ディレクトリ毎の設定を Nix で管理するためのライブラリ

などがある。これらを組み合わせることで、

* Linux、macOS を始め、各種 OS でのプロビジョニングを宣言的に行う
* 開発リポジトリに、必要なパッケージ群を宣言し、開発チームメンバーで同じ開発ツールを共有する
* 一部プロジェクトで、少し古いバージョンのツールを使いたい場合、一部ディレクトリ下のバージョンのみ異なるもので固定して使う
* 他のディレクトリに影響を及ぼすことなく、一部ディレクトリで試しに一時的にツールをインストールして、試す

といったことが行えるようになる。

僕の場合は、MacBook のセットアップを `Nix`_ で行っていて、その全ソースは https://github.com/mizunashi-mana/dotfiles にある。中身では、Git や Fish、VSCode といった日頃使うツールのインストールとセットアップから、Finder の設定や CapsLock を Control キーにマッピングする設定など macOS で最初に行う設定などを書いていて、これ一発通せば開発環境が整うようにしている。慣れると割りかし簡単にセットアップでき、がんばって macOS と格闘しなくてもいいし、設定を変えてもすぐ元に戻せるため重宝している。

今回はこの内、 `Nix Darwin`_、 `Home Manager`_ を使う場合の、macOS パッケージ管理を `Homebrew`_ から移行する方法を紹介する。

Nix への移行手順
--------------------

移行手順は、大きく以下のステップで行うと良い:

1. Homebrew を Nix でセットアップ
2. シェル設定を Nix へ移行
3. Homebrew パッケージを Nix パッケージに移行
4. Nix へ設定を完全移行

というのは、いきなり全てを Nix へ完全移行するのは、パッケージ数によっては結構大仕事になるからだ。まず、一旦試しに使ってみるという面でも徐々に移行できると良いだろう。

* 1 の手順では、まず Nix をインストールし使い始めつつ、パッケージや設定ファイルの管理は今までと同じ方法でできる。
* 2 の手順で、シェルだけ Nix に完全移行することで、環境変数などのセットアップが済み、Nix パッケージを使い始める準備ができる。
* 3 の手順で、Homebrew から脱却し、パッケージ管理のみ Nix に移行できる。
* 4 の手順は、気が向けばだが、設定ファイルなども Home Manager の管理下に置いておくと、Nix だけでプロビジョニングできるようになり、互いの連携も Home Manager がいい感じにしてくれる。

この流れで移行すれば、段階的に少しずつパッケージを Nix に移行できる。では、具体的に移行手順を見ていこう。

Homebrew を Nix でセットアップ
------------------------------------------------

まず、Nix をインストールする。インストールは、 `公式インストーラ <https://nixos.org/download/>`_ を使ってもいいが、 `非公式のインストーラ <https://github.com/DeterminateSystems/nix-installer>`_ を使うと、アンインストールが簡単にできるので、こちらを使うことをお勧めする。

Nix がインストールできたら、作業用のディレクトリを以下のように作る:

.. code-block:: shell
    :linenos: none

    mkdir ~/workdir
    cd ~/workdir

ここで、Nix の設定を行っていく。まず、 ``flake.nix`` を以下のように作成する:

.. code-block:: nix

    {
        description = "A flake to provision my environment";

        inputs = {
            nixpkgs = {
                url = "github:nixos/nixpkgs?ref=nixos-unstable";
            };

            home-manager = {
                url = "github:nix-community/home-manager";
                inputs.nixpkgs.follows = "nixpkgs";
            };

            nix-darwin = {
                url = "github:LnL7/nix-darwin";
                inputs.nixpkgs.follows = "nixpkgs";
            };
        };

        outputs = {
            self,
            nixpkgs,
            home-manager,
            nix-darwin,
        }: {
            darwinConfigurations = {
                "my-macbook" = nix-darwin.lib.darwinSystem {
                    # x86 macOS 使ってる場合は、"x86_64-darwin" を指定する
                    system = "aarch64-darwin";

                    modules = [
                        {
                            system = {
                                stateVersion = 5;
                            };
                        }
                        home-manager.darwinModules.home-manager
                    ];
                };
            };
        };
    }

この状態で、以下を実行する:

.. code-block:: shell
    :linenos: none

    nix run nix-darwin -- switch --flake .

これにより、Nix Darwin と Home Manager の導入が完了する。と言っても、これを実行したところで、特にシステム設定が変わることも、パッケージ導入が行われることもない。この設定を元に、肉付けを行うことで、macOS のシステム設定やパッケージの導入を行うことができる。

まず、Homebrew の設定を行ってみる。 ``zoxide`` というツールを題材にしてみる。元々 ``zoxide`` を Homebrew でインストールしている想定で、以下のように ``flake.nix`` を編集する:

.. code-block:: diff
    :linenos: none

                            system = {
                                stateVersion = 5;
                            };
    +
    +                       homebrew = {
    +                           enable = true;
    +
    +                           brews = [
    +                               "zoxide"
    +                           ];
    +                       };
                        }
                        home-manager.darwinModules.home-manager

この状態で、

.. code-block:: shell
    :linenos: none

    nix run nix-darwin -- switch --flake .

を実行すると、 ``zoxide`` が Homebrew 経由でインストールされる。現在インストールされている、Homebrew パッケージをこのように移していけば、Nix でひとまずパッケージ管理ができるようになる。

この状態でも、macOS の設定管理を Nix で行うことができる。例えば

.. code-block:: diff
    :linenos: none

                        {
                            system = {
                                stateVersion = 5;
    +
    +                           controlcenter = {
    +                               BatteryShowPercentage = true;
    +                           };
                            };

                            homebrew = {

と書いて、 ``nix run`` を実行すると、macOS のメニューバーでバッテリーのパーセンテージが表示されるようになる。他にも様々な設定が、 `Nix Darwin のマニュアル <https://nix-darwin.github.io/nix-darwin/manual/index.html>`_ から確認できる。

シェル設定を Nix へ移行
---------------------------------

これまでの手順で、Homebrew パッケージを Nix 経由でインストールしたり、macOS の設定を Nix 経由で管理したりはできる。ただ、このままでは、開発環境の再現性の問題は解決しない。その為には、Homebrew パッケージから Nix パッケージに移行する必要がある。ただ、Nix パッケージを利用するには、Nix の環境変数をシェルに読み込む必要がある。

Bash や Zsh であれば、Nix Darwin がデフォルトで連携してくれるので問題ない。Fish を使っている場合は、連携を有効化する必要がある。Fish を使っている場合、 ``flake.nix`` を以下のように変える:

.. code-block:: diff
    :linenos: none

                                    "zoxide"
                                ];
                            };
    +
    +                       programs.fish = {
    +                           enable = true;
    +                       };
                        }
                        home-manager.darwinModules.home-manager

この状態で ``nix run`` を実行すると、 ``/etc/fish/config.fish`` ファイルに Nix との連携用設定が書き込まれ、Nix パッケージのパスなどが環境変数に読み込まれるようになる。これで、Nix パッケージが Fish で使えるようになる。

これだけでも Nix パッケージは使えるようになるのだが、Home Manager との連携はされていないため、フルに恩恵を受けられない部分がある。例えば、この後 ``zoxide`` を Home Manager 管理に移行していくのだが、そこでシェルを Home Manager と連携しておくと、 ``zoxide`` とシェルの連携を Home Manager が自動で行ってくれる。他にも、追加の環境変数やエイリアス管理などを行ってくれるため、まずシェルを Home Manager とも連携しておくのをお勧めする。

Home Manager とシェルの連携は、例えば Fish の場合、 ``flake.nix`` を以下のように編集する:

.. code-block:: diff
    :linenos: none

                home-manager,
                nix-darwin,
            }:
    -       {
    +       let
    +           # 設定するマシンのホスト名を指定
    +           hostname = "my-macbook";
    +           # 設定するマシンのユーザ名を指定
    +           username = "my-username";
    +           # x86 macOS 使ってる場合は、"x86_64-darwin" を指定する
    +           system = "aarch64-darwin";
    +           homedir = "/Users/${username}";
    +
    +           pkgs = import nixpkgs {
    +               inherit system;
    +           };
    +       in {
                darwinConfigurations = {
    -               "my-macbook" = nix-darwin.lib.darwinSystem {
    -               # x86 macOS 使ってる場合は、"x86_64-darwin" を指定する
    -               system = "aarch64-darwin";
    +               "${hostname}" = nix-darwin.lib.darwinSystem {
    +                   inherit system pkgs;

                        modules = [
                            {
    ...
                                };
                            }
                            home-manager.darwinModules.home-manager
    +                       {
    +                           home-manager.useGlobalPkgs = true;
    +                           home-manager.useUserPackages = true;
    +                           home-manager.users."${username}" = {
    +                               home.stateVersion = "25.05";
    +
    +                               home.username = username;
    +                               home.homeDirectory = homedir;
    +
    +                               programs.fish = {
    +                                   enable = true;
    +                                   shellInit = builtins.readFile ./init.fish;
    +                               };
    +                           };
    +                       }
                        ];
                    };
                };

そして、 ``init.fish`` ファイルを同じ場所に作成し、そこに Fish の設定を書く。それから、 ``nix run`` を実行すると、Fish の設定が Home Manager 経由で適用される。

Zsh の場合は ``programs.zsh`` を使って同様に、Bash の場合は ``programs.bash`` を使って同様に設定できる。詳しい設定方法は、 `Home Manager のオプションマニュアル <https://nix-community.github.io/home-manager/options.xhtml>`_ を参照すると良い。

Homebrew パッケージを Nix パッケージに移行
------------------------------------------------------------

ここまでで、Homebrew パッケージを Nix パッケージに移行する準備が整うので、あとは順次移行しやすいものからパッケージ毎に移行していけばいい。 ``zoxide`` の場合は、以下のように ``flake.nix`` を編集する:

.. code-block:: diff
    :linenos: none

                                homebrew = {
                                    enable = true;

    -                               brews = [
    -                                   "zoxide"
    -                               ];
    +                               brews = [];
                                };

                                programs.fish = {
    ...
                                        enable = true;
                                        shellInit = builtins.readFile ./init.fish;
                                    };
    +
    +                               home.packages = [
    +                                   pkgs.zoxide
    +                               ];
                                };
                            }
                        ];

これで、 ``zoxide`` を Homebrew 経由のものから Nix パッケージのものに切り替えることができる。動作確認できたら、 ``brew remove zoxide`` で Homebrew 側のパッケージを削除しておくと良い。これで、 ``zoxide`` の Nix パッケージへの移行が完了する。このようにして、それぞれのパッケージを順次 Nix パッケージに移行していくことで、Homebrew から脱却してバージョンを固定し依存パッケージ込みで独立に管理できるようになる。

パッケージのバージョンは、 ``nix flake update`` を実行することで更新できる。これにより、 ``flake.lock`` ファイルが更新されるが、これをバージョン管理していくことで元のバージョンにすぐ戻せる。

Home Manager へ設定を完全移行
------------------------------------------------------------

ここまでで、Homebrew パッケージを Nix パッケージに移行できた。あとは、設定ファイルなども Home Manager 経由で管理してしまうと、Nix だけでプロビジョニングできるようになる。例えば、 ``zoxide`` を Home Manager に完全移行する場合、次のように書く:

.. code-block:: diff
    :linenos: none

                                        shellInit = builtins.readFile ./init.fish;
                                    };

    -                               home.packages = [
    -                                   pkgs.zoxide
    -                               ];
    +                               programs.zoxide = {
    +                                   enable = true;
    +                               };
                                };
                            }
                        ];

``home.packages`` では指定したパッケージのバイナリが使えるだけだったが、 ``programs.zoxide`` を指定することで Home Manager が連携してるシェルで ``zoxide init`` を実行し、 ``z`` コマンドなどを使えるようにしてくれる。他にも ``git`` や ``ssh`` を設定ファイル込みで管理できたりする。Home Manager で設定できるツールは、 `Home Manager のオプションマニュアル`_ から確認できる。

後は段階的に移行していけば、macOS の設定と各パッケージの設定を Nix だけで管理できるようになる。一部 GUI アプリケーションは、Homebrew 経由で入れたほうが楽な場合もあるが、そこも Nix Darwin 経由で Homebrew パッケージを管理できるので、他のマシンへの移行も格段に楽になるだろう。

まとめ
----------------------

今回は、Homebrew から Nix への移行方法を紹介した。Nix は、パッケージのバージョン固定や環境の分離が容易で、開発環境の再現性を高めることができる。Homebrew から Nix への移行は、段階的に行うことで、既存の環境を壊すことなくスムーズに行える。さらに、Nix のエコシステムを活用することで、macOS やツールの設定も一元管理できるようになり、ツールのバージョンも自由に戻せるようになる。移行はシェルの設定から始め、パッケージを順次 Nix に移行していくと良いだろう。

Nix、昔はかなり不安定でUX最悪みたいな感じだったが、ようやく色々時代が追いついてきたらしい。久しぶりに触ってみたけど、かなり快適に使えるようになってて、驚いた。[dotfiles](https://github.com/mizunashi-mana/dotfiles) も最近完全に Nix 移行して、手軽にいじれるようになって便利。興味持ったらぜひ使ってみてほしい。では、今日はこれで。
