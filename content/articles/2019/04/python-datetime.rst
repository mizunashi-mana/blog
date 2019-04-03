Python の datetime が難しい
===========================

:date: 2019-04-03
:tags: Python, 標準ライブラリ
:category: プログラミング

色々ハマったので，メモをしておく．

aware と naive
--------------

Python の ``datetime`` モジュールでは，日付情報を表現する ``date`` オブジェクト，日中時間を表現する ``time`` オブジェクト，そしてその両方の組である ``datetime`` オブジェクトが提供されている．そして，実は ``datetime`` オブジェクトは2種類ある．この扱いの差が分からなくてまずつまづいてた．

そもそもドキュメントをちゃんと読めという話なんだけど， https://docs.python.org/ja/3/library/datetime.html で普通に説明されている．端的に言えば，

* aware: 実時刻を表し，タイムゾーンや夏時間情報を持つ．
* naive: 時刻情報だけを表現する．タイムゾーンなどの情報は持たず，その時刻がどういう意味を持つかは処理する側に委ねられる．

の2種類があるらしい． aware か naive かは， ``tzinfo`` プロパティが ``None`` かどうかで判定できる． ``None`` なら naive になる．ところで，ほとんどの場合 ``datetime`` モジュールの API は naive なオブジェクトを製造する． ``datetime.datetime.now()`` でさえ naive な ``datetime`` オブジェクトを返す． aware なオブジェクトを取得したい場合明確にタイムゾーン情報を指定する必要がある．例えば，現在時刻を UTC で取得したい場合， ``datetime.datetime.utcnow()`` ではなく， ``datetime.datetime.now(datetime.timezone.utc)`` と書く．前者は naive なオブジェクトになり，後者は aware なオブジェクトになる．

aware / naive の相互変換と tzinfo
---------------------------------

それぞれのオブジェクトの相互変換は， ``replace`` を使うことで以下のように可能::

  def aware_to_naive(d):
    return d.replace(tzinfo=None)

  def naive_to_aware(d, tzinfo):
    return d.replace(tzinfo=tzinfo)

3.5 までは， naive と aware の区別はかなり厳密で，例えば ``astimezone`` は naive に対しては呼べなかった．これは， 3.6 で実行環境のローカルなタイムゾーンによる時刻だと解釈して呼ばれるよう変更されている．

なお，呼び方によって naive と aware のどちらが返ってくるかが別れる API も幾つかある． ``strptime`` はタイムゾーン指定があると aware なオブジェクトを返し，ないと naive なオブジェクトを返す::

  >>> datetime.datetime.strptime('2019-01-01T00:00:00Z', '%Y-%m-%dT%H:%M:%S%z')
  datetime.datetime(2019, 1, 1, 0, 0, tzinfo=datetime.timezone.utc)
  >>> datetime.datetime.strptime('2019-01-01T00:00:00', '%Y-%m-%dT%H:%M:%S')
  datetime.datetime(2019, 1, 1, 0, 0)

ロケール込みの情報としてパースしたい場合，タイムゾーン情報を明示的に付与して aware に変換する必要がある::

  >>> datetime.datetime.strptime('2019-01-01T00:00:00', '%Y-%m-%dT%H:%M:%S') \
  ...   .replace(tzinfo=datetime.timezone.utc)
  datetime.datetime(2019, 1, 1, 0, 0, tzinfo=datetime.timezone.utc)

``tzinfo`` は抽象クラスになっていて，自分でカスタムしたオブジェクトを作れるようになっている．標準的にはタイムゾーン情報を元にした ``tzinfo`` オブジェクトの実装を提供する ``timezone`` クラスを使っておけば問題ない． ``timezone`` オブジェクトは UTC からの時間差から作成できる::

  >>> datetime.datetime.strptime('2019-01-01T00:00:00', '%Y-%m-%dT%H:%M:%S') \
  ...   .replace(tzinfo=datetime.timezone(datetime.timedelta(hours=9)))
  datetime.datetime(2019, 1, 1, 0, 0, tzinfo=datetime.timezone(datetime.timedelta(seconds=32400)))

まとめ
------

ちゃんとドキュメントは読もうと思いました (まる) ． ``datetime`` モジュール， 3.5 - 3.7 の間に結構変わってて厳しい気持ちになる． ``fromisoformat`` が使えるのは 3.7 からだし， ``strptime`` の ``%Z`` で「:」付きの表現をパースしてくれるのも 3.7 からっぽい．

世の中は厳しいなあって感じでした．
