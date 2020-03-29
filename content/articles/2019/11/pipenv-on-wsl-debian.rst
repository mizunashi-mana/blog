pipenv を WSL/Debian で使う際の注意事項
=======================================

:tags: Python, Debian, WSL
:category: 環境構築

久しぶりに Windows 上で環境構築をすることになったんだけど，その際 Pipenv をインストールするのにちょっと躓いたので，その備忘録．

Pipenv を入れた時のエラー
-------------------------

Windows 上で開発する際は， WSL/Debian を使っている．で，新しい環境でもこれで行くことにして， Pipenv を Python3 環境で使いたかったので，次のようにして入れた::

  sudo apt update
  sudo apt install -y python3-pip
  pip3 install pipenv

これで，問題なく入ったように見えたんだけど，実際に実行してみると以下のような例外を吐いていた::

  $ pipenv install
  ['Traceback (most recent call last):\n', ...]
  Traceback (most recent call last):
    ...
    File "/usr/lib/python3.7/subprocess.py", line 1522, in _execute_child
      raise child_exception_type(errno_num, err_msg, err_filename)
  OSError: [Errno 8] Exec format error: '/mnt/c/.../AppData/Local/Microsoft/WindowsApps/python.exe'

  During handling of the above exception, another exception occurred:

  Traceback (most recent call last):
    ...
    File "~/.local/lib/python3.7/site-packages/pipenv/vendor/vistir/misc.py", line 162, in _create_subprocess
      sys.stderr.write("Error %s while executing command %s", exc, " ".join(cmd._parts))
  TypeError: write() takes exactly one argument (3 given)

原因は詳しく調べてないけど，エラーメッセージからおそらく， Python3 のインタプリタを使わずに Python2 のを探してるんじゃないかと思ってる． Debian は現在 Python2 から Python3 の移行期で，デフォルトでは ``python`` は Python2 のインタプリタ， ``python3`` が Python3 のインタプリタというようになっている．で， ``pipenv`` は Python3 の Pip で入れたにもかかわらず，内部で ``python`` という名前でインタプリタを探してたりするんじゃないかなと考えている [#notice-details-for-this-issue]_ ．

解決策
------

で，じゃあなんか解決する方法があるのかだけど，単純には次を実行すればいいっぽい::

  pip3 uninstall -y pipenv
  sudo apt install -y pipenv

または，問題が起きてるのが ``virtualenv`` の環境を作る際に選択する Python のバージョンを間違えてることに起因してるっぽいので，一回環境を作ってその環境を使い続ける分には ``pip3`` で入れたものでも大丈夫みたい． Debian のパッケージもあまり詳しく見てないけど， ``pipenv`` パッケージはその辺うまいことするパッチでも当ててるってことかな？ とりあえず，これで問題なく動いてるので，まいっかってなってる．

まとめ
------

また詰まるかもしれないので，忘れる前にとりあえず書いといた．気が向けば原因調査したりするかも．後，今回は WSL で問題が起きたけど，普通に Debian 環境だと起きる問題だったりするかも．こちらからは以上です．

.. [#notice-details-for-this-issue] なんか原因知ってる人いたら教えてほしい．
