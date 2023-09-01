malloc と併用可能なアロケータを作る
===============================================

:tags: Linux, glibc, メモリ
:category: プログラミング

大規模で、ユーザ入力などの外部要因により左右されるプログラムでは、動的なメモリ管理が必要になる場合が多い。特に GC が普及した今日では、多くのプログラムにおいて基本的な機能と言えるだろう。このメモリ管理の機構として、多くのプログラミング言語実装では libc の ``malloc`` / ``free`` がデファクト的に使われている。一般に、プログラムの動的なメモリの割り当てと解放を抽象化した機構はメモリアロケータ、または単にアロケータと呼ばれる。libc malloc はその標準的なものとして、システムアロケータと呼ばれたりする。特に、ほとんどの環境では libc のデファクト標準で glibc が使われることから、glibc malloc が事実上の標準システムアロケータであると言ってよいだろう。このため、libc malloc は数多くのライブラリやプログラミング言語ランタイムで使われている。

さて、そんな glibc malloc だが、汎用性を重んじて作られているため、メモリ管理に少し手を加えたい場面ではパフォーマンス面とのバランスも考えると適さない場合もある。そのような場合に一からアロケータを作成したい場合もあるが、既存資産の相互運用も考えると malloc 互換の API を用意する、もしくは glibc malloc と併用可能にしたい場合もある。特に、libc が静的にリンクされる場合のことなども考えると、glibc malloc と併用可能な状況を作ることが望ましい場合がある。しかし、アロケータを glibc malloc と併用可能にするには多少テクニックが必要だ。今回は、x86 64bit Linux 環境で、glibc malloc と併用可能なアロケータを実装するテクニックについて紹介する。

glibc malloc の実装
---------------------------

まずは基本的なところから。さて、そもそも何気なく使われている ``malloc`` / ``free`` だが、この中身はどうなっているのだろう？ glibc malloc の役割は、大雑把には、

必要に応じたヒープ領域の拡張
    メモリ管理で主に使われる領域を足りなくなったら必要に応じて OS に拡張してもらう。

使用可能な領域からのメモリ領域の切り出し
    まだ使用していない領域を管理し、そこから要求された分を切り出して必要に応じてヘッダ情報を載せアプリケーション側に管理を委譲する。これは誤解している人が多いと思うが、一般的にアロケータの「メモリ割り当て」と呼ばれる動作は、実際の物理メモリへの新たなデータ書き込みを伴うものでないことが多い。これについては詳しくは後述するが、一般的にアロケータの役割は実際のメモリデータ管理と配置というよりは、メモリ番地の空き管理と予約に近い。予約済みのものをアプリケーション側に提供し、後はよしなに利用してくれ、利用終わったら言ってくれたら必要に応じて掃除して空室としてまた予約可能にしとくよという感じだろう。

使用終了した領域を使用可能にする
    アプリケーション側から使用終了の告知があった領域を、必要に応じて後処理をし、使用可能な領域に戻す。

の3つだ。では、それぞれ少し詳しく見ていこう。

まず、そもそもだが、現代的なアーキテクチャと OS では仮想メモリという仕組みのサポートがある。通常私たちが触っているメモリの番地とは仮想的なものであり、実際の物理的なメモリにアクセスする際は CPU が OS の変換テーブルを使って実際の対応する物理メモリ番地に変換してからアクセスを行う。仮想メモリの番地は、物理的なメモリに必ずしもマッピングされているとは限らないし、物理的なメモリに連続して配置されているとも限らない。現代的にはページング方式という仮想メモリ領域の物理メモリへの展開方法が多くの場合採用されており、ざっくばらんによくある説明として、仮想メモリ空間をページという小さな固定サイズの単位に分解して、実際にデータ置かれてるページだけを物理メモリに連続してマッピングさせるというものだ。まあ、この辺の詳細は気が向いたらまた別に書くかもしれないが、詳細に立ち入るとそれだけで2、3個記事が書けるので、今回は大枠そんな感じの仕組みになってることさえ抑えて貰えばよいだろう。詳しく知りたければ「詳解Linuxカーネル」とかを読んでもらうのがいいんじゃないだろうか。とりあえず、このような仕組み上仮想メモリは物理メモリと同じサイズである必要はないし、使用領域を連続させる必要もない。

さて、ではプログラム実行時に私たちがいじっているメモリについてだが、まず Linux ではプロセス毎にそれぞれ仮想メモリ空間を作っており、その空間の中でもレイアウトを決めている。仮想メモリ空間のサイズは、アーキテクチャが扱える最大サイズが基本で、つまり 32bit アーキテクチャなら 2^32B = 4GB、64bit アーキテクチャなら 2^64B = 16EB となる [#virtual-memory-max-address]_。ただ、この内カーネルの使用領域があるため、実際にユーザ空間で自由に使用できるのは x86 32bit では 3GB まで [#virtual-memory-introduction-by-konsulko]_、x86 64bit では通常 128TB まで (一部 64PB までいけるものもある) [#virtual-memory-layout-of-x86-64]_ となる。このユーザ空間の領域もある程度配置場所が決まっている。基本的には、まずプロセス管理用のデータや静的に確保されたデータ領域が置かれた後、それ以降が動的に管理されるメモリ領域となる。動的に管理される領域は中間に ``mmap`` と呼ばれるシステムコールによるメモリマッピング領域が置かれ、それより手前がヒープ領域、それ以降がスタック領域となる。よくある図で書くと以下のようになる:

.. image:: {attach}impl-custom-alloc-integrated-with-malloc/process-user-memory-layout.svg
   :alt: プロセスのユーザ空間メモリレイアウト配置

ヒープ領域は ``sbrk`` というシステムコールにより program break を移動させることで使える領域が拡張され、動的なメモリオブジェクトが置かれる。スタックはサブルーチンの呼び出し時に stack pointer が移動することで拡張され、サブルーチンの制御情報が書かれる。メモリマッピング領域は ``mmap`` システムコールが呼び出された時に必要に応じて拡張され、ファイルのメモリへのマッピング、もしくは動的なメモリオブジェクトがページ単位で置かれる。glibc malloc がメモリ割り当てに使うのはこの内ヒープ領域とメモリマッピング領域になる。

glibc malloc は大雑把には、 ``malloc`` 時要求サイズに対する閾値で切り出し元の領域を変える。要求サイズが閾値以下であれば、そのサイズに合う切り出し領域をヒープ領域から見つけてきて返し、閾値より大きければ ``mmap`` を使ってメモリマッピング領域にページ単位で割り当てる。ヒープ領域が足りなければ、 ``sbrk`` システムコールによって program break を移動させる。 ``mmap`` での割り当てはページ単位になるので、あまりにも割り当てサイズが小さいとメモリ領域を無駄に消費してしまうことになる。なので、全て ``mmap`` で凌ぐのはメモリ容量が増えた現代でも割と愚策。では、逆になぜ全部ヒープ領域でやらないかだが、その前にざっくりヒープ領域の管理アルゴリズムの概要を見ていこう。

さて、サイズに合う切り出し領域をヒープ領域から見つけてきて返すというのは、言うのは簡単だが実際に実装する際はいくつかステップが必要だ。まず、どの領域が既に使用済みで、どこからどこまでが使用可能かを特定する必要がある。ただ、私たちは数個のレジスタに記録した情報だけからこれを探さないといけない。愚直にはこの特定作業は、使用箇所をメモっておいてその使用箇所を先頭から調べていけば良い。実際初期の glibc malloc はそんな感じのことをしていたらしい。ただ、現代はメモリはギガ単位が普通であり、メモリ割り当ての度にそんなことをしてるとプログラムの実行速度にかなり影響が出る。そこで近代では、ある程度のサイズ毎にリストを作り、そこにそのサイズ毎の空きブロックをぶら下げておきそれをメモリ割り当ての際は返し、返却されたらまたリストにぶら下げると言うことをしている。これによりわざわざ全ての使用箇所を線形探索する必要はなくなるわけだ。ただ、可能性のある全サイズでリストを作るのはそれはそれでメモリ管理的に無駄が大きい。特に再利用される可能性のあるブロックのサイズというのはある程度傾向があり、ある程度大きくなってくるとサイズはかなりばらけてくる。そこで、再利用される可能性の高いある程度小さいブロックはヒープ領域でリストに紐づけて管理し、ある程度大きいブロックは ``mmap`` で OS に要求して直接取ることで両方使用可能領域の線形探索を避けることができる。またこれは断片化の回避にもつながる。単純にヒープ領域を先頭から切り出して割り当て、いらなくなったら解放といったことをやっていると、メモリの使用可能領域はかなり穴ボコ状態になって、連続した領域のサイズが限られてくる。その為、メモリの空きは 1G あるのに、1kB の連続領域はない為割り当てができないといったことになりかねない。小さいサイズのブロックの割り当て領域をある程度決めて整理した割り当てができればこのような状況を比較的回避しやすくなる。これが、ヒープ領域とメモリマッピング領域が併用されている理由になる。ここら辺の詳細は、kosaki さんの `malloc の旅 <https://www.slideshare.net/kosaki55tea/glibc-malloc>`_ というスライドが大変分かりやすい。参考にすると良いだろう。

mmap と無名マッピング
---------------------------

さて、glibc malloc がヒープ領域とメモリマッピング領域を併用して動的なメモリ管理を行なってることは分かった。問題はこの glibc malloc のメモリ管理を邪魔しない形で、新たにメモリ管理機構を載せるにはどうすればいいかだ。ヒープ領域を使ってしまうと、諸に glibc malloc のメモリ管理と色々衝突してしまうことになる。今回紹介する方法は、簡単にはヒープ領域はダメでもメモリマッピング領域で ``mmap`` から領域取得する分には glibc malloc の動きを阻害することがないので、 ``mmap`` で独自のヒープ領域を切り出してこようというものだ。その話に入る前に、 ``mmap`` の無名マッピングと呼ばれる機能についてもう少し詳しく見ておく。これは、glibc malloc の ``mmap`` による領域確保でも使われている機能になる。

``mmap`` システムコールは基本的にファイルをメモリにマッピングすることを想定して作られたシステムコールだ。有名どころで言うと、共有ライブラリの読み込みが ``mmap`` により行われる。しかし、動的メモリ管理における需要を受けて、ファイルに紐づかないマッピング領域作成モード、 ``MAP_ANONYMOUS`` が導入されて以来こちらもかなり色々な場面で利用されるようになっている。 ``mmap`` は作成するマッピングに対して幾つかモードを指定できる。 ``MAP_ANONYMOUS``、無名モードはそのモードの中の一つだ。glibc malloc が ``mmap`` でメモリ割り当てを行う際は、当然その領域はファイルには紐づかないので無名モードで領域が確保されることになる。

``mmap`` システムコールは、以下の API を持つ [#man-of-mmap]_:

.. code-block:: c

    void *mmap(void addr, size_t length, int prot, int flags, int fd, off_t offset);

一つ目の ``addr`` はマッピングする領域のヒントとなるアドレスを受け取る。アドレスが ``NULL`` なら適当なアドレスを持ってくるし、 ``NULL`` でないならその周辺から切り出してくる。 ``length`` は領域のサイズになる。 ``prot``、 ``flags`` は後述する。 ``fd`` は、マッピング元のファイルディスクリプタで、無名モードの場合は ``-1`` を渡す。 ``offset`` はマッピング元ファイルのオフセットを指定でき、無名モードの場合は ``0`` を渡す。 ``flags`` では、マッピングモードのビット集合を指定する。詳細は、マニュアルを見てもらいたいが、今回扱いたいものだけ紹介しておくと、

``MAP_PRIVATE``
    他プロセスと共有しないマッピング領域を、遅延書き込み (copy-on-write) で作成する。プライベートモードで作成された領域は、 ``mmap`` 完了時は物理メモリには載らず、仮想メモリ上での予約のみがされた状態になる。そして、初回アクセス時ページフォールト例外を受けて初めて物理領域へのページ登録が起こる。なお、単に読み込みアクセスだけを行う場合は特殊な0埋め領域を参照するようになっており、書き込みアクセス時初めて専用の物理メモリスペースが確保される。これが遅延書き込みの機能になる。

``MAP_ANONYMOUS``
    無名モードを指定するフラグ。このモードの場合、ファイルには紐づかず、単なるメモリ領域として使用できる。

``MAP_FIXED``
    ``addr`` 引数で指定されたアドレスについて、ヒントではなく正確にそのアドレスを始点とする領域を確保する。ただし、 ``addr`` 引数はページサイズできちんとアラインされてる必要があり、またアーキテクチャによっては他にも条件が科されることがあり移植性はあまりよくない。そのため、あまり使用は推奨されていない。

のようなフラグやその他色々が指定できる。 ``prot`` はマッピング領域の保護権限を指定する。アクセス権限を与えない ``PROT_NONE``、もしくは以下による空でないビット集合を指定できる:

``PROT_EXEC``
    実行可能。バッファオーバーフローなどを利用してこの領域に意図しない書き込みをされたりすると、セキュリティホールの元となるのでこの権限を設定する領域の管理には注意が必要。

``PROT_READ``
    読み込み可能。

``PROT_WRITE``
    書き込み可能。

glibc malloc では、 ``MAP_ANONYMOUS|MAP_PRIVATE`` モードで、かつ ``PROT_READ|PROT_WRITE`` 権限の領域確保が使われている [#glibc-sysmalloc-mmap-impl]_。同じように無名モードで ``mmap`` で確保された領域を使えば、glibc malloc の管理機構に迷惑をかけることなく独自の動的なメモリ領域を確保できる。

ただ、課題もある。ヒープ領域は仮想メモリアドレスにおいては連続的であるため、それを利用したアドレス計算テクニックなどが使えるが、 ``mmap`` で随時メモリ領域をメモリマッピング領域に確保していく場合、常に連続的に領域を拡張できるとは限らない。なぜなら、glibc malloc などが確保した領域が間に挟まってくるかもしれないからだ。今回紹介するのは、この問題を ``PROT_NONE`` 権限でのメモリマッピングにより回避する方法だ。

``PROT_NONE`` 権限で ``mmap`` を実行すると、アクセス権限を持たないメモリマッピング領域が確保される。これは、物理メモリへの参照を持たずアクセスするとエラーが起きる一見役に立たない機能に見えるが、あることに応用ができる。それは、仮想メモリアドレスの予約である。つまり、事前に ``PROT_NONE`` 権限である程度の仮想メモリ領域を ``mmap`` しておけば、その仮想メモリのアドレスは使用済みとなり、それ以降の ``mmap`` で使用されることはないというわけである。この予約は、 ``munmap`` しない限り解除されない。また、メモリの権限は ``mprotect`` システムコール、または ``mmap`` 時 ``MAP_FIXED`` モードを指定してそのアドレスへの上書きマッピングを行うことで更新できる。つまり、事前に ``PROT_NONE`` で膨大なメモリ領域を予約しておき、その後随時必要になったら ``mprotect`` で ``PROT_READ|PROT_WRITE`` 権限を付与した領域を増やしていくことで、program break 移動により拡張できるヒープ領域と同じような管理を行うことができるということだ。仮想メモリは 64bit 環境であれば物理メモリより通常かなり空間が大きいので、4分の1ぐらい予約しておいても支障はほとんどない。予約しても単にアドレスが他で使えなくなるだけで物理メモリに載るわけでもないし、残りの4分の3は自由に使えるわけで、例えば 64bit だと 32TB の仮想メモリ領域を予約領域として、残りの 96TB の仮想メモリ領域を他のメモリ管理機構が自由に使える。これで問題が出るサーバは普通ないだろう。これを利用することで、新たなヒープ領域を作るメモリ管理を2重に独立した領域で作ることができるというわけだ。

これは実は Go 言語の 64bit 向けのランタイムで使われてる手法で、これにより cgo などで glibc malloc との共存ができるようになっている。32bit でこの方法を使うと流石に少し支障が出てしまうのが難点なのと、Linux での実装とアーキテクチャのメモリ管理機構の仕組みに強く依存しているので移植性が悪いという問題はあるが、64bit のアーキテクチャで広く流通している x86 64、ARM64 ではこの手法が使え、Windowsでも Win API で似たような機能が搭載されているため大体の範囲で適用でき、しかも既存の仕組みをあまり壊さずそこまで複雑な手段を必要とせず使える点が有用だ。

実装してみる
---------------------------

実際に、簡単なアロケータを上の手法を使って Rust で実装してみる。まず、 ``mmap`` のラップ API を用意しておく:

.. code-block:: rust

    mod sys {
        use std::{error::Error, ptr::NonNull};

        pub type AnyNonNull = NonNull<libc::c_void>;

        /// ページサイズを取得する。mmap で確保する領域はこのページサイズの倍数になってる必要がある。
        pub unsafe fn get_pagesize() -> Result<usize, Box<dyn Error>> {
            let pagesize = libc::sysconf(libc::_SC_PAGE_SIZE);
            if pagesize < 0 {
                Err(std::io::Error::last_os_error().into())
            } else {
                Ok(pagesize as usize)
            }
        }

        /// 仮想メモリ空間の一部を長さ分だけ予約する。
        pub unsafe fn reserve(len: usize) -> Result<AnyNonNull, Box<dyn Error>> {
            let ptr = libc::mmap(
                std::ptr::null_mut(),
                len,
                libc::PROT_NONE,
                libc::MAP_ANONYMOUS | libc::MAP_PRIVATE,
                -1,
                0
            );
            if ptr == libc::MAP_FAILED {
                Err(std::io::Error::last_os_error().into())
            } else {
                Ok(NonNull::new_unchecked(ptr))
            }
        }

        #[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
        pub enum CommitStrategy {
            Mprotect,
            MmapFixed,
        }

        /// 予約された領域の一部を使用できるようにする。
        pub unsafe fn commit(
            addr: AnyNonNull,
            len: usize,
            prefer_strategy: CommitStrategy,
        ) -> Result<CommitStrategy, Box<dyn Error>> {
            if prefer_strategy <= CommitStrategy::Mprotect {
                // mprotect は Linux 4.9 以上でないと使えない。
                let result = libc::mprotect(
                    addr.as_ptr(),
                    len,
                    libc::PROT_READ | libc::PROT_WRITE,
                );
                if result == 0 {
                    return Ok(CommitStrategy::Mprotect);
                }
            }

            // MAP_FIXED での上書きマッピングは本来非推奨。
            // ただ、mprotect が使えない環境は存在するのでフォールバックしておく。
            let ptr = libc::mmap(
                addr.as_ptr(),
                len,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_ANONYMOUS | libc::MAP_PRIVATE | libc::MAP_FIXED,
                -1,
                0,
            );
            if ptr == libc::MAP_FAILED {
                Err(std::io::Error::last_os_error().into())
            } else {
                Ok(CommitStrategy::MmapFixed)
            }
        }

        #[allow(unused)]
        #[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
        pub enum DecommitStrategy {
            Mprotect,
            MmapFixed,
        }

        /// 予約された領域の一部の使用をやめる。予約されたままだが、物理メモリへのマッピングは解かれる。今回は使用しない。
        #[allow(unused)]
        pub unsafe fn decommit(
            addr: AnyNonNull,
            len: usize,
            prefer_strategy: DecommitStrategy
        ) -> Result<DecommitStrategy, Box<dyn Error>> {
            if prefer_strategy <= DecommitStrategy::Mprotect {
                let result = libc::mprotect(
                    addr.as_ptr(),
                    len,
                    libc::PROT_NONE,
                );
                if result == 0 {
                    return Ok(DecommitStrategy::Mprotect);
                }
            }

            let ptr = libc::mmap(
                addr.as_ptr(),
                len,
                libc::PROT_NONE,
                libc::MAP_ANONYMOUS | libc::MAP_PRIVATE | libc::MAP_FIXED,
                -1,
                0,
            );
            if ptr == libc::MAP_FAILED {
                Err(std::io::Error::last_os_error().into())
            } else {
                Ok(DecommitStrategy::MmapFixed)
            }
        }

        /// 仮想メモリ空間の一部を使用可能な状態で切り出す。
        pub unsafe fn alloc(len: usize) -> Result<AnyNonNull, Box<dyn Error>> {
            let ptr = libc::mmap(
                std::ptr::null_mut(),
                len,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_ANONYMOUS | libc::MAP_PRIVATE,
                -1,
                0,
            );
            if ptr == libc::MAP_FAILED {
                Err(std::io::Error::last_os_error().into())
            } else {
                Ok(NonNull::new_unchecked(ptr))
            }
        }

        /// マッピング済みの領域のマッピングを解除する。新たに alloc / reserve で使用できるようになる。
        pub unsafe fn release(addr: AnyNonNull, len: usize) -> Result<(), Box<dyn Error>> {
            let result = libc::munmap(addr.as_ptr(), len);
            if result != 0 {
                Err(std::io::Error::last_os_error().into())
            } else {
                Ok(())
            }
        }
    }

`libc crate <https://docs.rs/crate/libc/latest>`_ を通してシステムコールを今回使う範囲で使いやすいように呼んでる。ま、それぞれの詳細はシステムコールのマニュアルを読んでもらうとして、基本的な用途は以下の感じ:

``get_pagesize``
    ページサイズを取得する API。``reserve`` や ``commit``、 ``alloc`` を呼ぶ際、指定する長さがページサイズでアラインされてる、つまりページサイズの倍数になっている必要があるため、その計算のために用意している。

``reserve``
    最初に glibc malloc のメモリ管理と独立した連続領域の予約を行うための API。アクセス権限はなく、物理メモリにもマッピングされないが指定した長さ分仮想メモリアドレスが予約された領域の先頭のポインタが返ってくる。

``commit``
    予約した領域を使う際に、物理メモリへのマッピングを行えるようにする API。 ``MAP_PRIVATE`` での予約なので、遅延書き込み、つまり書き込みが起こるまで実際には独自の物理メモリへのマッピングは起きないが、気分的にはメモリ割り当てのような立ち位置。なお、通常は ``mprotect`` を使うのだが、 ``mprotect`` は Linux 4.9 で導入された割と新参のシステムコールなので広く使われてる言語では移植性などを考え ``mmap`` の ``MAP_FIXED`` モードへのフォールバックが用意されているか、Go ではそっちしか使ってないみたいな状況。今回は、 ``mprotect`` が使えるならそっちを使う、使えないなら以降は ``MAP_FIXED`` を使うという小細工を施せるようにしている。なお、厳密には ``mprotect`` のエラーコードをちゃんとみて、対応してない場合のエラーコードのみフォールバックを発動させるのがお行儀が良い。

``decommit``
    今回は色々実装が面倒なので使用しないが、 ``commit`` した領域の物理メモリへのマッピングを解除する、つまり使わなくなった領域をメモリに載せるのをやめる API。呼んで直後にマッピングが解除されるとは限らないが、少なくともページの追い出しで優先的に追い出されるようにはなる。 ``commit`` と同じようなフォールバックを設けている。

``alloc``
    一旦予約するステップを踏まないで、直接メモリ領域を使用可能状態で確保する API。今回のアロケータ実装では、glibc malloc に少しならって、小さいブロックは連続領域に、ある程度大きいブロックはページサイズでアラインしてこの ``alloc`` で直接確保するようにする。

``release``
    ``reserve`` や ``alloc`` で確保した領域を解放し、その仮想メモリアドレスを再度利用できるようにする API。今回は ``alloc`` で確保した領域のみに使用する。

これらの API を使って、簡単なシングルスレッドのみ対応のアロケータを作ってみる。今回実装するアロケータの概要は以下のようになる:

* 1kB 以下のブロックは連続領域に確保しサイズのクラス毎にリストで管理、それ以外のブロックは ``alloc``、 ``release`` で管理する。
* ブロックにはヘッダを付け、連続領域に確保した場合はブロックサイズのクラス、それ以外の場合はブロックサイズを持っておく。

まず幾つかパラメータを宣言しておく:

.. code-block:: rust

    const MAX_HEAP_SIZE: usize = 2 << 40;
    const SUBHEAP_COUNT: usize = 7;

    const fn block_size_of_subheap(class_of_subheap: usize) -> usize {
        2 << (class_of_subheap + 3)
    }

    const MAX_BLOCK_SIZE: usize = block_size_of_subheap(SUBHEAP_COUNT - 1);

    /// * `alignment` - A power of 2.
    const fn aligned_size(original: usize, alignment: usize) -> usize {
        let mask = alignment - 1;
        original + (original.reverse_bits() & mask)
    }

``MAX_HEAP_SIZE`` は今回はお遊びなので適当に大きな値を設定している。プロダクションに載せる際は、ちゃんと物理メモリの上限を取ってきてそこから数値を決めるのがいいんじゃないだろうか。 ``SUBHEAP_COUNT`` はサイズのクラス分けの数を表している。流石に全てのサイズに対してフリーリストを作るのは色々無駄なので、通常サイズをクラス分けしてある程度無駄が生じるのを許容してフィットするクラスの最大サイズで領域確保が行われる。今回もそれに倣い、ブロックサイズ :math:`m` に対して :math:`2^{(c_m - 1)} < m \leq 2^{c_m}` を満たす :math:`c_m` を :math:`m` のクラスと考え、:math:`2^{c_m}` サイズのブロックとして確保する。ここら辺は、例えば Go だと 64 個ぐらいクラスがあり、クラス分けももう少し工夫してある。サイズからのクラスの特定に要する時間とよく使われるサイズ種別への対応のバランスを取るのが大事。 ``block_size_of_subheap`` はクラスの最大ブロックサイズを返す。 ``MAX_BLOCK_SIZE`` は連続領域に確保されるブロックの最大サイズとなる。 ``aligned_size`` はユーティリティで、 ``original <= x * alignment`` となる最小の ``x`` を計算する。主にページサイズの倍数となるようサイズ調節するのに使用する。

次にアロケータ用のデータ構造を定義する:

.. code-block:: rust

    pub struct Allocator {
        // immutable
        pagesize: usize,
        heap_end: sys::AnyNonNull,

        // mutable
        free_lists: [*mut FreeHeader; SUBHEAP_COUNT],
        active_heap_end: sys::AnyNonNull,
        commited_heap_end: sys::AnyNonNull,

        prefer_commit_strategy: sys::CommitStrategy,
    }

    struct Header {
        size_or_class_of_subheap: usize,
    }

    struct FreeHeader {
        #[allow(unused)]
        header: Header,
        next: *mut FreeHeader,
    }

    impl Allocator {
        pub unsafe fn init() -> Result<Self, Box<dyn Error>> {
            let pagesize = sys::get_pagesize()?;
            assert!(MAX_HEAP_SIZE % pagesize == 0);

            let heap_begin = sys::reserve(MAX_HEAP_SIZE)?;
            let heap_end = NonNull::new_unchecked(heap_begin.as_ptr().add(MAX_HEAP_SIZE));
            let free_lists = [std::ptr::null_mut(); SUBHEAP_COUNT];

            Ok(Self {
                pagesize,
                heap_end,
                free_lists,
                active_heap_end: heap_begin,
                commited_heap_end: heap_begin,
                prefer_commit_strategy: sys::CommitStrategy::Mprotect,
            })
        }

        ...
    }

アロケータは主に、フリーブロックのリストと、連続領域の使用済み地点を持つ。

``free_lists`` がフリーブロックのリストで、 ``free`` されたブロックはここに追加され、次回の ``alloc`` で再利用される。リストは、生ポインタ使った単方向リストで実装している。この辺は、Rust 標準アロケータに頼らないで、かつメモリレイアウトを簡潔にするため。

``active_heap_end`` は現在使用している領域の最後を表す。この地点からなら新たにブロック用に切り出しが可能。ただ、 ``active_heap_end`` はページサイズでアラインされてるとは限らないので、実際に ``commit`` でアクセス権限をつけてる領域とはズレてる可能性がある。 ``commit`` が完了してる地点を表すのが ``commit_heap_end``。ブロック用に切り出す領域がまだコミット済みじゃない領域まで必要とする場合は、まずその領域分ページサイズアラインされた領域を ``commit`` し、その後使用する分だけ切り出すことになる。

アロケータ初期化の時点では、まず連続領域用の仮想メモリアドレスの予約が走り、空のフリーリストと空の連続領域からスタートとなる。その後の割り当て ``alloc`` と解放 ``free`` の実装は以下のようになる:

.. code-block:: rust

    impl Allocator {
        pub unsafe fn alloc<T: Sized>(&mut self) -> Result<NonNull<T>, Box<dyn Error>> {
            self.alloc_by_size(size_of::<T>())
        }

        pub unsafe fn alloc_by_size<T>(&mut self, len: usize) -> Result<NonNull<T>, Box<dyn Error>> {
            if len <= MAX_BLOCK_SIZE {
                for class_of_subheap in 0..SUBHEAP_COUNT {
                    if len <= block_size_of_subheap(class_of_subheap) {
                        return self.alloc_on_subheap(class_of_subheap);
                    }
                }
                self.alloc_on_subheap(SUBHEAP_COUNT - 1)
            } else {
                self.alloc_on_external(len)
            }
        }

        pub unsafe fn free<T>(&mut self, ptr: NonNull<T>) -> Result<(), Box<dyn Error>> {
            let allocated_ptr = (ptr.as_ptr() as *mut libc::c_void)
                .offset(- (size_of::<Header>() as isize));
            let allocated_ptr = NonNull::new_unchecked(allocated_ptr as *mut Header);

            let size_or_class_of_subheap = allocated_ptr.as_ref().size_or_class_of_subheap;
            if size_or_class_of_subheap <= MAX_BLOCK_SIZE {
                let class_of_subheap = size_or_class_of_subheap;
                self.free_on_subheap(allocated_ptr, class_of_subheap)
            } else {
                let size = size_or_class_of_subheap;
                self.free_on_external(allocated_ptr, size)
            }
        }
    }

基本的にどちらもブロックサイズが連続領域のブロックの最大サイズ以下かで分岐し、それぞれの処理を行う。連続領域以外での処理の方が簡単なので、まずそっちを見ておく:

.. code-block:: rust

    impl Allocator {
        ...

        unsafe fn alloc_on_external<T>(&mut self, len: usize) -> Result<NonNull<T>, Box<dyn Error>> {
            let allocated_size = aligned_size(len + size_of::<Header>(), self.pagesize);
            let mut allocated_ptr: NonNull<Header> = sys::alloc(allocated_size)?.cast();
            *allocated_ptr.as_mut() = Header {
                size_or_class_of_subheap: allocated_size,
            };
            Ok(allocated_ptr.cast())
        }

        unsafe fn free_on_external(&mut self, addr: NonNull<Header>, size: usize) -> Result<(), Box<dyn Error>> {
            sys::release(addr.cast(), size)
        }

        ...
    }

ヘッダつけてページアラインしたサイズの領域を ``sys::alloc`` で確保して返し、ヘッダからサイズ特定して ``sys::release`` で解放してるだけ。ま、こっちはいいだろう。連続領域での確保の方は以下のようになる:

.. code-block:: rust

    impl Allocator {
        ...

        unsafe fn alloc_on_subheap<T>(&mut self, class_of_subheap: usize) -> Result<NonNull<T>, Box<dyn Error>> {
            match NonNull::new(self.free_lists[class_of_subheap]) {
                None => {
                    let allocated_ptr = self.extend_active_heap_end(class_of_subheap)?;
                    let allocated_ptr: NonNull<libc::c_void> = allocated_ptr.cast();
                    Ok(NonNull::new_unchecked(allocated_ptr.as_ptr().add(size_of::<Header>()) as *mut T))
                }
                Some(free_ptr) => {
                    self.free_lists[class_of_subheap] = free_ptr.as_ref().next;
                    let used_ptr: NonNull<libc::c_void> = free_ptr.cast();
                    Ok(NonNull::new_unchecked(used_ptr.as_ptr().add(size_of::<Header>()) as *mut T))
                }
            }
        }

        unsafe fn free_on_subheap(&mut self, addr: NonNull<Header>, class_of_subheap: usize) -> Result<(), Box<dyn Error>> {
            let mut addr: NonNull<FreeHeader> = addr.cast();
            addr.as_mut().next = self.free_lists[class_of_subheap];
            self.free_lists[class_of_subheap] = addr.as_ptr();
            Ok(())
        }

        ...
    }

こっちは、クラスのフリーリストに再利用可能なブロックがあればそれを使い、なければ使用可能領域を必要なだけ広げて切り出すのが割り当てになり、フリーリストに単に繋げるのが開放になる。更新処理でのロックなどは取ってないので、シングルスレッド専用。マルチスレッド対応する際は、排他処理以外にも色々考えるべきことがあるので、今回は考えないことにしておく。最後に使用可能領域の拡張は、以下のようになる:

.. code-block:: rust

    impl Allocator {
        ...

        unsafe fn extend_active_heap_end(&mut self, class_of_subheap: usize) -> Result<NonNull<Header>, Box<dyn Error>> {
            let allocated_size = size_of::<Header>() + block_size_of_subheap(class_of_subheap);
            let new_active_heap_end = NonNull::new_unchecked(self.active_heap_end.as_ptr().add(allocated_size));
            if self.heap_end < new_active_heap_end {
                return Err(format!("Failed to extend heap size.").into());
            }

            if self.commited_heap_end < new_active_heap_end {
                let committed_size = aligned_size(
                    new_active_heap_end.as_ptr().offset_from(self.active_heap_end.as_ptr()) as usize,
                    self.pagesize,
                );
                self.prefer_commit_strategy = sys::commit(self.commited_heap_end, committed_size, self.prefer_commit_strategy)?;
                self.commited_heap_end = NonNull::new_unchecked(self.commited_heap_end.as_ptr().add(committed_size));
            }

            let mut allocated_ptr: NonNull<Header> = self.active_heap_end.cast();
            self.active_heap_end = new_active_heap_end;

            *allocated_ptr.as_mut() = Header {
                size_or_class_of_subheap: class_of_subheap,
            };
            Ok(allocated_ptr)
        }
    }

まずヘッダ付きの領域サイズを計算し、コミット済みの領域が足りなければまず追加のコミットを行う。その後、使用可能領域を広げその領域をブロック用の領域として返す。こんな感じで glibc malloc と併用可能な自前アロケータを実装できる。全体のコードは、https://github.com/mizunashi-mana/sample-alloc-by-freelist においておいた。今回の例だとそもそも連続領域は実は必要ないので、あくまで参考用という感じではある。

まとめ
-----------

というわけで、 ``mmap`` の ``MAP_PRIVATE|MAP_ANONYMOUS`` モードを利用した glibc malloc と併用可能な動的メモリ管理の実装方法について紹介した。仮想メモリ空間が少し小さい 32bit 環境では別の方法を考えた方がいいだろうが、x86 64、ARM64 環境の Linux であればこの方法で問題は起きないだろう。もちろん、連続空間がそもそも必要ないアロケーションアルゴリズムも世の中には色々あるので、その際はアドレスの予約までしないで ``mmap`` で適宜領域を広げていっても良い。言語のランタイムを独自で作る場合はメモリ配置を工夫するだけでパフォーマンスが強く改善する場合もあるし、glibc malloc はメトリクスが少ないので独自にメトリクスを仕込みたい場合もある。そういった場合にアロケータを自作することは、よくあるだろう。その際にこの手法が役に立てば幸いだ。

これまであまり malloc の裏側がどうなってるのかなどあまり気にしたことなかったし、OS が頑張ってどうにかしてくれてるだろうぐらいに考えていたので、結構中身がデータ構造による工夫でどうにかなっていることが知れてよかった。おかげで動的メモリ管理については結構解像度上がった感がある。この辺は普段のプログラミングとか GC 作りとかにも応用していきたいなという感じ。ま、この辺最大限頑張ろうと思うとアーキテクチャ毎に工夫が必要になり、結構泥臭い部分も多いが。では、今回はこれで。

.. [#virtual-memory-max-address] 正確には 16EB まで仮想メモリアドレスとして使える 64bit アーキテクチャは稀で、大体は 48bit、つまり 256TB ぐらいまでが限界になる。これは 64bit フルサポートは実益がないのに対して、アドレス変換テーブルの保存が大変だから。まあ、ただ 256TB までのサポートだと最近微妙になってきつつあったので、57bit、128PB サポートのものも出てきつつあるようだ。
.. [#virtual-memory-introduction-by-konsulko] https://elinux.org/images/b/b0/Introduction_to_Memory_Management_in_Linux.pdf
.. [#virtual-memory-layout-of-x86-64] https://www.kernel.org/doc/html/v5.8/x86/x86_64/mm.html
.. [#man-of-mmap] https://www.man7.org/linux/man-pages/man2/mmap.2.html
.. [#glibc-sysmalloc-mmap-impl] https://sourceware.org/git/?p=glibc.git;a=blob;f=malloc/malloc.c;h=d0bbbf371048ee8aa8a30c03b189cb268b8ad9e4;hb=HEAD#l2420
