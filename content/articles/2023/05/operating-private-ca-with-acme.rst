ACME 対応 Private CA を手軽に立てる
=============================================

:tags: 証明書, 認証局, セキュリティ
:category: 運用

今日のネットワーク通信では、様々な中継ポイントを介すことが多い。これにより、様々な繋がりを作ることができるようになっている。しかしその反面、中継ポイントには様々な運用者が挟まることになり、盗聴や改竄といった不正を行いやすい状況になっている。そのため、通信の秘匿化、接続相手の認証が重要になる。そのような状況で、覇権を握っているのが TLS という仕組みになる。

TLS 通信では、大まかには、接続先から提供される証明書が妥当なものかを検証し、その情報を元に暗号通信のための情報を秘密裏に共有し、秘匿化された暗号通信路を確保する。証明書の検証が接続相手の認証に相当し、その後の暗号通信路の確保が通信の秘匿化に対応することになる。今回は、このうち証明書の検証の部分のみを扱っていく。認証は事前の何らかの情報共有が必要になる。単純にはそれぞれの接続先に対して認証情報を事前に確保しておくと認証は行えるが、今日の接続先が無数にある状況では、認証情報の管理方式に問題が出てくる。そこで、TLS 通信では、一部の認証局と呼ばれる機関の認証情報のみを事前に共有しておき、接続先は証明書を事前に認証局に認証してもらった上で交付し、認証局の認証部分を検証するようにすることで共有する認証情報を抑えつつ、柔軟で拡張性のある暗号通信の仕組みを提供する。

さて、TLS 通信の規格は全て公開されており、もちろん認証局も自分で開設することが可能である。認証局からの証明書の発行は元来は手動操作だったが、昨今は発行操作が規格化され、より認証局の運用は簡単なものになっている。今回はこの ACME というプロトコルで規格化された証明書発行フローの紹介と、そのフローに対応した認証局の開設、運用を caddy という Web サーバで行う方法を紹介する。

TLS 証明書と CA
----------------------

TLS 通信における接続相手の認証は、サーバから送られたサーバ認証証明書データとその証明書データを使って作られた署名データによって行われる。これはデジタル署名の応用によるもので、TLS 1.3 では署名アルゴリズムとして RSA / ECDSA / EdDSA などの幾つかのアルゴリズムが使えるが、これらのアルゴリズムには共通して

* 公開鍵、秘密鍵のペアを発行し、
* 秘密鍵から生成した署名データが公開鍵で検証可能、すなわち署名データが必ず元データから対となる秘密鍵で生成したものであることを判定でき、
* 公開鍵からは秘密鍵が現実的に類推不可能である

という特徴がある。この特徴を利用することで、公開鍵を証明書データに入れておき、証明書データの公開鍵と対となる秘密鍵でクライアントからのリクエストを署名して署名データとして送ることで、通信相手を認証することができると言うわけだ。ただ、これには前提として、送られてきた証明書データが送り主のものであることをクライアントが保証できる必要がある。

単純にはこれには現存する全ての証明書をクライアントが把握していれば良いわけだが、現状 TLS が普及し、無数のサーバが存在する中で、全ての証明書を把握することはリソース的に難しいし、また管理するリソースが増えることで安全性の欠落を招きかねない。そこで重要となるのが PKI、日本語で公開鍵基盤と呼ばれる仕組みである。PKI とはまさに、公開鍵がその持ち主のものであることを保証する仕組みのことを指す総称である。TLS において最も広く用いられている PKI 規格として X.509 PKI [#rfc-5280]_ がある。X.509 では、PKI の機能を証明書の連鎖によって実現する。つまり、ある通信先の識別子が書かれた証明書を別の秘密鍵で署名しておき、そちらの対となる公開鍵をクライアントが知れるようにしておくことで、署名された証明書そのものは通信の際に取得しつつ、証明書に署名している公開鍵さえ事前に取得しておけばその公開鍵を使って取得した証明書が通信先が発行したものであることを保証できる。しかも複数の証明書の署名を1つの秘密鍵で行えば検証用に管理する公開鍵の数自体が減らせるということになる。このような証明書自体の署名検証に使う公開鍵をさらに別の証明書に埋め込んでおき、その証明書もまた別の証明書の公開鍵で署名するといったことを繰り返すことにより、無数にある末端の証明書をその証明書の連鎖で繋がれた数個の公開鍵だけ管理していれば検証することが可能になるというわけだ。X.509 はその証明書の連鎖の方式と、その連鎖を実現させるための証明書データなどの形式を規格化したものになる。

X.509 のアーキテクチャには、以下のエンティティが登場する:

末端エンティティ
    PKI により保証された証明書を使用するシステム。

認証局 (Certification Authority、略して CA)
    証明書に署名する第三者機関。

登録局 (Registration Authority、略して RA)
    証明書申請者の本人性を審査・確認する機関。CA が行う場合もあるが、他の機関に委任されることもある。

CRL 発行者 (CRL Issuer)
    証明書失効リスト (Certificate Revocation List、略して CRL) を生成して署名する機関。CA が行う場合もあるが、他の機関に委任されることもある。

リポジトリ (Repository)
    証明書と CRL を管理、配布するシステム。

X.509 ではこれらのエンティティにより、以下のやりとりが行われることによって、PKI の管理を行う:

登録
    CA に認証先を登録するプロセス。これは、RA を介して行われることもある。認証先は、末端エンティティもしくは別の認証局である。

証明書発行
    登録された認証先に対して CA が証明書を発行するプロセス。証明書が発行されるとリポジトリに登録される。

証明書更新
    古くなった証明書の代わりに CA から新しく証明書を発行するプロセス。公開鍵・秘密鍵のペアは定期的に更新されなければならないため、このプロセスは定期的に発生する。

証明書失効要求
    証明書を何らかの理由により有効期限前に失効させるべきであることを CA に通知するプロセス。証明書は有効期限を持ち、通常はその期限の満了まで有効だが、秘密鍵の漏洩など何らかの理由によりその有効期限前に証明書を失効させる必要が生まれる場合がある。その場合に CA に通知し、CA は CRL にその証明書を載せることを CRL 発行者に依頼し、リポジトリの更新とともに CRL を通じて証明書が失効されたことが証明書検証者に通知される。

X.509 はそれぞれのプロセスは規格化せず、それぞれの CA に任せるが、発行する証明書及び CRL の形式と、それらの検証方法を規格化している。まずは証明書の構造から見ていく。X.509 証明書は、

署名対象証明書 (To Be Signed Certificate、略して TBS Certificate)
    証明書本体の情報。証明書の持ち主及び証明書を発行した CA の情報、認証先の公開鍵、有効期限などの情報が含まれている。載せられる情報については後述する。

署名アルゴリズム識別子 (Signature Algorithm)
    署名に使うアルゴリズムの識別子。

デジタル署名 (Signature Value)
    署名対象証明書に対して署名アルゴリズム識別子に示される署名アルゴリズムで生成された CA による署名。これにより、署名対象証明書が改竄されることを防ぎつつ、CA から発行されたものであることが保証できる。

の3つにより構成される。署名対象証明書には、以下の情報が載っている:

バージョン
    X.509 のバージョン。X.509 は v1 から今までバージョン更新が2回あり、最新の v3 が現在最も多く使用されているバージョンである。この記事では、v3 しか扱わないので、基本ここは 3 が埋まってると思って良い。

シリアル番号
    証明書発行者毎に各証明書に振られる一意な正整数による番号。つまり、各証明書は発行者とシリアル番号により一意に定まる。

署名アルゴリズム識別子
    これは証明書発行の際与えられた識別子で、証明書本体の署名に使われたアルゴリズムの識別子と一致している必要がある。

発行者 (Issuer)
    証明書発行元 (認証元) を識別する名前。空でない key-value による列で定義される。標準的には、国 (country)、組織 (organization)、組織単位 (organizational unit)、識別名の修飾子 (distinguished name qualifier)、州または地方の名前 (state or province name)、共通名 (common name) を key とした属性が埋められるようにすることが規定されており、これらの属性が使われていることが多い。

有効性 (Validity)
    証明書の有効期間。証明書が有効化される日時 (not before)、証明書の有効期間が終了する日時 (not after) が設定できる。

主体者 (Subject)
    証明書の公開鍵により保証されるエンティティを識別する名前。発行者名と同じく、空でない key-value による列で定義される。ただ、後述するが昨今は個人情報保護の観点もあって、共通名のみ埋める CA も結構出てきている。一般には、TLS 用の末端エンティティの場合共通名にドメイン名が用いられるのが一般的で、これを前提に HTTPS の意味論が規定されていた時代もあったが、今は代わりに後述する subjectAltName という拡張フィールドで明確にドメイン名を指定するのが一般的で、共通名の検証は HTTPS では禁じられている [#rfc-6125-sec6.4.4]_。CA 証明書の場合は、発行する証明書の発行者と同じにする必要がある。

主体者公開鍵情報 (Subject Public Key Info)
    主体者の検証に使う公開鍵と検証用の署名アルゴリズムの識別子。主体者が TLS 用の末端エンティティであれば TLS で使う公開鍵が、CA であれば発行された証明書の署名検証用に使う公開鍵が書かれることになる。

拡張 (Extensions)
    key-value により載せられる任意の拡張で、CA用の独自の情報を埋め込んだりできる。X.509 内で標準的に規定されており、証明書検証に影響を与える拡張もある。

他に一意識別子が載せられるが、これは現在は非推奨で特に要件は設けられておらず、載せても載せなくても良い。他に標準拡張として

鍵識別子
    鍵を識別する識別子。発行者、主体者それぞれに対して付加できる。

鍵使用目的 (Key Usage)
    証明書に含まれる公開鍵の使用目的。複数指定可能。デジタル署名 (digitalSignature)、否認防止 (nonRepudiation)、鍵暗号化 (keyEncipherment)、データ暗号化 (dataEncipherment)、鍵交換 (keyAgreement)、証明書署名 (keyCertSign)、CRL署名 (cRLSign)の中から選ぶ。鍵交換についてはさらにデータ暗号化 (encipherOnly)、データ復号 (decipherOnly)の制御が指定できる。

証明書ポリシー (Certificate Policies)
    証明書及びその証明書による証明書パスに連なる証明書の利用目的を表すデータ。今回は詳しく触れない。

ポリシーマッピング (Policy Mappings)
    CA 証明書において、ポリシー間のマッピングを規定するデータ。今回は詳しく触れない。

主体者別名 (Subject Alternative Name)
    主体者の別名。複数指定可能。複数の末端エンティティについて、一つの証明書を使いたい時に用いられる。TLS 用証明書では慣例的に、主体者名に証明書識別のための共通名を設定し、主体者別名でドメイン名 (DNS 名) を指定するのが一般的。

基本制約 (Basic Constraints)
    主体者が CA であるか、及びこの証明書が含まれる証明書パス中の中間証明書の最大の数 (最大長) を規定。拡張と言いながらこの拡張は必ず含める必要がある。つまり、X.509 では、CA 証明書かどうかを必ず規定する必要があるため、証明書署名用の証明書を、末端エンティティ用に使うことはできない。証明書パスの最大長は、CA 証明書である場合のみ指定できる。

ポリシー制約 (Policy Constraints)
    CA 証明書の場合に、受け入れ可能なポリシーを規定する。今回は詳しく触れない。

拡張鍵使用目的 (Extended Key Usage)
    鍵使用目的に加えて追加で指定できる公開鍵の使用目的。サーバ認証 (serverAuth)、クライアント認証 (clientAuth)、コード署名 (codeSigning)、メール保護 (emailProtection)、電子タイムスタンプ (timeStamping)、OCSP署名 (OCSPSigning)が指定できる。また特別に、任意の目的に利用可能 (anyExtendedKeyUsage) であることが指定できる。

CRL 配布点 (CRL Distribution Points)
    CRL の取得方法を規定する。複数指定可能。いくつか項目が指定できるが、基本的には CRL がダウンロードできる URI だけが指定されていることが多い。

任意ポリシー禁止 (Inhibit anyPolicy)
    証明書ポリシーで anyPolicy の使用を禁じることを規定する。今回は詳しく触れない。

機関情報アクセス (Authority Information Access)
    証明書の発行者に関するサービスへのアクセス方法。CA 証明書の配布 URL や、OCSP レスポンダの URL などが書いてある場合が多い。

他にもいくつか標準拡張はあるが、主なものはこんな感じ。これらの情報を元に、X.509 では証明書パスが与えられた時に以下を検証する:

* 証明書パスの出だしはトラストアンカーに載っている、つまり既に知っている証明書である。
    * 一般にこの出だしの証明書をルート証明書といい、その証明書の発行元 CA をルート CA という。ブラウザや OS には信頼するルート CA の証明書のデータベースがインストールされており、これが X.509 におけるトラストアンカーとなっている。
    * ルート CA は発行者が自分自身になっている自己署名になっている場合が多い。このような他の認証局の認証によらずトラストアンカーによる認証を受ける必要がある CA を、自己認証 CA と呼んだりもする。
* 証明書パスに載っている証明書は、出だしを除いて、その証明書の発行者名と一つ手前の証明書の主体者名が一致しており、一つ手前の証明書の主体者公開鍵で署名が検証できる。
* 証明書パスに載っている証明書は、検証時刻においてCRL に載っておらず全て有効期間内である。
* 証明書パスに載っている証明書は、拡張で規定される制約を満たす。

これにより、TLS においてルート CA までの証明書パスをサーバから取得すれば、ルート CA 証明書を持っているだけで、X.509 PKI と主体者名による判定により公開鍵が主体者のものであることが保証できるというわけだ。

HTTPS サイトの X.509 証明書を見るビュワーは、大体どのブラウザにも搭載されてるので、結構手軽にみることができる。OpenSSL コマンドで見ることも可能だ。例えば、我らが github.com の証明書は以下のように見ることができる::

    $ openssl s_client -connect github.com:443 -showcerts
    CONNECTED(00000005)
    depth=2 C = US, O = DigiCert Inc, OU = www.digicert.com, CN = DigiCert Global Root CA
    verify return:1
    depth=1 C = US, O = DigiCert Inc, CN = DigiCert TLS Hybrid ECC SHA384 2020 CA1
    verify return:1
    depth=0 C = US, ST = California, L = San Francisco, O = "GitHub, Inc.", CN = github.com
    verify return:1
    write W BLOCK
    ---
    Certificate chain
    0 s:/C=US/ST=California/L=San Francisco/O=GitHub, Inc./CN=github.com
    i:/C=US/O=DigiCert Inc/CN=DigiCert TLS Hybrid ECC SHA384 2020 CA1
    -----BEGIN CERTIFICATE-----
    MIIFajCCBPGgAwIBAgIQDNCovsYyz+ZF7KCpsIT7HDAKBggqhkjOPQQDAzBWMQsw
    CQYDVQQGEwJVUzEVMBMGA1UEChMMRGlnaUNlcnQgSW5jMTAwLgYDVQQDEydEaWdp
    Q2VydCBUTFMgSHlicmlkIEVDQyBTSEEzODQgMjAyMCBDQTEwHhcNMjMwMjE0MDAw
    MDAwWhcNMjQwMzE0MjM1OTU5WjBmMQswCQYDVQQGEwJVUzETMBEGA1UECBMKQ2Fs
    aWZvcm5pYTEWMBQGA1UEBxMNU2FuIEZyYW5jaXNjbzEVMBMGA1UEChMMR2l0SHVi
    LCBJbmMuMRMwEQYDVQQDEwpnaXRodWIuY29tMFkwEwYHKoZIzj0CAQYIKoZIzj0D
    AQcDQgAEo6QDRgPfRlFWy8k5qyLN52xZlnqToPu5QByQMog2xgl2nFD1Vfd2Xmgg
    nO4i7YMMFTAQQUReMqyQodWq8uVDs6OCA48wggOLMB8GA1UdIwQYMBaAFAq8CCkX
    jKU5bXoOzjPHLrPt+8N6MB0GA1UdDgQWBBTHByd4hfKdM8lMXlZ9XNaOcmfr3jAl
    BgNVHREEHjAcggpnaXRodWIuY29tgg53d3cuZ2l0aHViLmNvbTAOBgNVHQ8BAf8E
    BAMCB4AwHQYDVR0lBBYwFAYIKwYBBQUHAwEGCCsGAQUFBwMCMIGbBgNVHR8EgZMw
    gZAwRqBEoEKGQGh0dHA6Ly9jcmwzLmRpZ2ljZXJ0LmNvbS9EaWdpQ2VydFRMU0h5
    YnJpZEVDQ1NIQTM4NDIwMjBDQTEtMS5jcmwwRqBEoEKGQGh0dHA6Ly9jcmw0LmRp
    Z2ljZXJ0LmNvbS9EaWdpQ2VydFRMU0h5YnJpZEVDQ1NIQTM4NDIwMjBDQTEtMS5j
    cmwwPgYDVR0gBDcwNTAzBgZngQwBAgIwKTAnBggrBgEFBQcCARYbaHR0cDovL3d3
    dy5kaWdpY2VydC5jb20vQ1BTMIGFBggrBgEFBQcBAQR5MHcwJAYIKwYBBQUHMAGG
    GGh0dHA6Ly9vY3NwLmRpZ2ljZXJ0LmNvbTBPBggrBgEFBQcwAoZDaHR0cDovL2Nh
    Y2VydHMuZGlnaWNlcnQuY29tL0RpZ2lDZXJ0VExTSHlicmlkRUNDU0hBMzg0MjAy
    MENBMS0xLmNydDAJBgNVHRMEAjAAMIIBgAYKKwYBBAHWeQIEAgSCAXAEggFsAWoA
    dwDuzdBk1dsazsVct520zROiModGfLzs3sNRSFlGcR+1mwAAAYZQ3Rv6AAAEAwBI
    MEYCIQDkFq7T4iy6gp+pefJLxpRS7U3gh8xQymmxtI8FdzqU6wIhALWfw/nLD63Q
    YPIwG3EFchINvWUfB6mcU0t2lRIEpr8uAHYASLDja9qmRzQP5WoC+p0w6xxSActW
    3SyB2bu/qznYhHMAAAGGUN0cKwAABAMARzBFAiAePGAyfiBR9dbhr31N9ZfESC5G
    V2uGBTcyTyUENrH3twIhAPwJfsB8A4MmNr2nW+sdE1n2YiCObW+3DTHr2/UR7lvU
    AHcAO1N3dT4tuYBOizBbBv5AO2fYT8P0x70ADS1yb+H61BcAAAGGUN0cOgAABAMA
    SDBGAiEAzOBr9OZ0+6OSZyFTiywN64PysN0FLeLRyL5jmEsYrDYCIQDu0jtgWiMI
    KU6CM0dKcqUWLkaFE23c2iWAhYAHqrFRRzAKBggqhkjOPQQDAwNnADBkAjAE3A3U
    3jSZCpwfqOHBdlxi9ASgKTU+wg0qw3FqtfQ31OwLYFdxh0MlNk/HwkjRSWgCMFbQ
    vMkXEPvNvv4t30K6xtpG26qmZ+6OiISBIIXMljWnsiYR1gyZnTzIg3AQSw4Vmw==
    -----END CERTIFICATE-----
    1 s:/C=US/O=DigiCert Inc/CN=DigiCert TLS Hybrid ECC SHA384 2020 CA1
    i:/C=US/O=DigiCert Inc/OU=www.digicert.com/CN=DigiCert Global Root CA
    -----BEGIN CERTIFICATE-----
    ...
    -----END CERTIFICATE-----
    ---
    Server certificate
    subject=/C=US/ST=California/L=San Francisco/O=GitHub, Inc./CN=github.com
    issuer=/C=US/O=DigiCert Inc/CN=DigiCert TLS Hybrid ECC SHA384 2020 CA1
    ---
    No client certificate CA names sent
    Server Temp Key: ECDH, X25519, 253 bits
    ---
    SSL handshake has read 2801 bytes and written 351 bytes
    ---
    New, TLSv1/SSLv3, Cipher is AEAD-CHACHA20-POLY1305-SHA256
    ...
    ---
    read R BLOCK
    read R BLOCK
    ^D

この場合の証明書パスは

::

    depth=2 C = US, O = DigiCert Inc, OU = www.digicert.com, CN = DigiCert Global Root CA
    verify return:1
    depth=1 C = US, O = DigiCert Inc, CN = DigiCert TLS Hybrid ECC SHA384 2020 CA1
    verify return:1
    depth=0 C = US, ST = California, L = San Francisco, O = "GitHub, Inc.", CN = github.com
    verify return:1

になっており、それぞれの depth に続く key-value が主体者名を表す。ルート CA が DigiCert のもので、そこから DigiCert TLS CA 用の証明書が発行され、さらに DigiCert TLS CA から github.com の末端エンティティ証明書が発行されている。ルート CA と末端エンティティの間の証明書を持つ CA を中間 CA と呼んだりする。今回は中間 CA 一つの証明書が 3 つ連なる証明書パスとなっている。 ``-----BEGIN CERTIFICATE-----``、 ``-----END CERTIFICATE-----`` で囲まれたデータが証明書データになる。証明書データは、ASN.1 という表現により規定されてるのだが、その符号化方式は幾つかある。一般的にはバイナリ表現として DER、表示用の表現として DER を Base64 エンコードする符号化形式 PEM が一般に用いられている。TLS では DER でやりとりが行われ、OpenSSL コマンドはそれを PEM 形式で表示しているというわけだ。この符号化されたデータの中身を見たい場合は、証明書データを以下の要領で OpenSSL に食わしてやれば良い::

    $ cat github.com.pem
    -----BEGIN CERTIFICATE-----
    MIIFajCCBPGgAwIBAgIQDNCovsYyz+ZF7KCpsIT7HDAKBggqhkjOPQQDAzBWMQsw
    ...
    -----END CERTIFICATE-----
    $ openssl x509 -in github.com.pem -text -noout
    Certificate:
        Data:
            Version: 3 (0x2)
            Serial Number:
                0c:d0:a8:be:c6:32:cf:e6:45:ec:a0:a9:b0:84:fb:1c
        Signature Algorithm: ecdsa-with-SHA384
            Issuer: C=US, O=DigiCert Inc, CN=DigiCert TLS Hybrid ECC SHA384 2020 CA1
            Validity
                Not Before: Feb 14 00:00:00 2023 GMT
                Not After : Mar 14 23:59:59 2024 GMT
            Subject: C=US, ST=California, L=San Francisco, O=GitHub, Inc., CN=github.com
            Subject Public Key Info:
                Public Key Algorithm: id-ecPublicKey
                    Public-Key: (256 bit)
                    pub:
                        04:a3:a4:03:46:03:df:46:51:56:cb:c9:39:ab:22:
                        cd:e7:6c:59:96:7a:93:a0:fb:b9:40:1c:90:32:88:
                        36:c6:09:76:9c:50:f5:55:f7:76:5e:68:20:9c:ee:
                        22:ed:83:0c:15:30:10:41:44:5e:32:ac:90:a1:d5:
                        aa:f2:e5:43:b3
                    ASN1 OID: prime256v1
                    NIST CURVE: P-256
            X509v3 extensions:
                X509v3 Authority Key Identifier:
                    keyid:0A:BC:08:29:17:8C:A5:39:6D:7A:0E:CE:33:C7:2E:B3:ED:FB:C3:7A

                X509v3 Subject Key Identifier:
                    C7:07:27:78:85:F2:9D:33:C9:4C:5E:56:7D:5C:D6:8E:72:67:EB:DE
                X509v3 Subject Alternative Name:
                    DNS:github.com, DNS:www.github.com
                X509v3 Key Usage: critical
                    Digital Signature
                X509v3 Extended Key Usage:
                    TLS Web Server Authentication, TLS Web Client Authentication
                X509v3 CRL Distribution Points:

                    Full Name:
                    URI:http://crl3.digicert.com/DigiCertTLSHybridECCSHA3842020CA1-1.crl

                    Full Name:
                    URI:http://crl4.digicert.com/DigiCertTLSHybridECCSHA3842020CA1-1.crl

                X509v3 Certificate Policies:
                    Policy: 2.23.140.1.2.2
                    CPS: http://www.digicert.com/CPS

                Authority Information Access:
                    OCSP - URI:http://ocsp.digicert.com
                    CA Issuers - URI:http://cacerts.digicert.com/DigiCertTLSHybridECCSHA3842020CA1-1.crt

                X509v3 Basic Constraints:
                    CA:FALSE
                1.3.6.1.4.1.11129.2.4.2:
    ......-....c.K..6.!...;`Z#.)N.3GJr...F..m..%......QG..P..+.....G0E. .<`2~ Q....}M...H.FWk..72O%.6....!...~.|..&6..[...Y.b .mo..
        Signature Algorithm: ecdsa-with-SHA384
            30:64:02:30:04:dc:0d:d4:de:34:99:0a:9c:1f:a8:e1:c1:76:
            5c:62:f4:04:a0:29:35:3e:c2:0d:2a:c3:71:6a:b5:f4:37:d4:
            ec:0b:60:57:71:87:43:25:36:4f:c7:c2:48:d1:49:68:02:30:
            56:d0:bc:c9:17:10:fb:cd:be:fe:2d:df:42:ba:c6:da:46:db:
            aa:a6:67:ee:8e:88:84:81:20:85:cc:96:35:a7:b2:26:11:d6:
            0c:99:9d:3c:c8:83:70:10:4b:0e:15:9b

ACME による証明書発行
-------------------------

さて、X.509 PKI により、認証局の運営とそこから発行された証明書の管理がちゃんとできれば、技術的に TLS 通信の前提となる PKI の機能を実現できることは分かった。ただ問題は、認証局の運営をどうするかである。ここら辺は長年そこまで厳密な規格はなされておらず、各 CA がブラウザなどの要望を踏みながら各自が手続きを整備している。一般的なフローは、

1. 証明書に載せる公開鍵、主体者名などの情報が書かれた証明書署名要求 (Certificate Signing Request、略して CSR) を作成
2. CA に CSR と本人確認情報などを渡す
3. CA (RA に委任している場合は RA) はドメインの所有者であることや本人確認情報などの審査を行う
4. 審査が通ったら CSR を元に証明書を発行

という感じだ。審査内容は、CA によってまちまちだが、主要な CA・ブラウザにより構成された CA/Browser Forum である程度の基準値が設けられており [#cabforum-servercert]_、以下の区分がある:

ドメイン認証 (Domain Validated)
    エンティティのドメインを、証明書署名要求者が所有していることが確認された証明書。そのドメインにメールを送る、トークンを TXT レコードで設定するなど確認方法はいくつかある。

実在組織認証 (Organization Validated)
    主体者名に指定された組織が実在し、そのエンティティを所有していることが確認された証明書。

実在個人認証 (Individual Validated)
    主体者名に指定された個人が実在し、そのエンティティを所有していることが確認された証明書。

拡張認証 (Extended Validated)
    CA/Browser Forum の最初の仕事で、標準化された法的な実在性確認などを含む認証プロセスを通して主体者が確認された証明書。

ここら辺は色々歴史的な紆余曲折があり、ドメイン認証は PKI としての機能には沿ったもので運用コストも安い反面ドメインを持っていれば誰でも取れるためフィッシングの温床になり、それを問題視して拡張認証が生まれたが、世の中の人はサイトの所有者情報をそこまでちゃんと見ていないことが分かって実はフィッシング対策にそれほどならなかったなど、まあ色々ある。最近の潮流としては、主体者情報に組織、個人の情報を含める場合はちゃんと実在認証をやる、PKI 技術にただ乗っかりたい場合は主体者情報はその証明書を使用するエンティティを識別できる情報を共通名に入れるだけにしておいて、ドメイン認証は最低限やるが、実在確認までは必要ないといった感じに落ち着いてきたように思う。さて、その潮流において、ドメイン認証だけであれば別に大層なことをやる必要はなく、手続きをサーバ間のやり取りのみで自動化できる。そのような中で生まれたのが Let's Encrypt というサービスである。Let's Encrypt はドメイン認証証明書の発行手続きを完全自動化することで、CA 業務コストをサーバ管理のみにし、TLS を無償で使用できるようにすることを目指したサービスである。そして、このサービス提供の中で生み出され規格化された証明書自動管理プロトコルが Automatic Certificate Management Environment、略して ACME になる。

ACME は HTTPS 上でサーバ-クライアントが JSON POST で話す割と今時のプロトコルで、

アカウント作成
    サービス規約に同意し、CA のサービスを受け取る、クライアントのユーザ単位リソースを作成する。

証明書の注文
    証明書が必要なことを CA に伝え、発行手続きを開始する。

識別子の承認
    CA から証明書発行に必要な認証手続きを受ける。

証明書の発行
    注文して認証された証明書を取得する。

証明書の失効
    証明書を有効期間前に失効させたい旨を CA に伝える。

の機能をサポートしている。初めて証明書を発行するまでの流れは以下のようになる:

1. アカウント作成
    * このステップでは、証明書を管理するためのサービスユーザ単位を発行する。
    * クライアントは、認証兼リクエスト署名検証用の公開鍵、サービス規約への同意の意思と、連絡先などの情報をサーバに渡すと、サーバからアカウントIDが発行される。
    * クライアントは発行されたアカウント ID と秘密鍵を保存しておき、この後の手順では ID と秘密鍵から生成された署名をリクエストに含めるようにする。
2. 証明書の注文
    * このステップでは、要求される証明書の識別子の認証と証明書発行の手続きに入る。
    * クライアントは発行したい証明書の主体者識別子をサーバに送り、サーバは認証手続きを開始し、クライアントに認証状態を取得できる URL と証明書発行手続きに進む URL を返す。
    * 識別子の認証は単体で事前に行うこともできる。
    * 識別子自体は、現在はドメイン名のみ対応している。これについては詳しくは後述する。
3. 識別子の承認
    * このステップでは、識別子が指し示すエンティティがクライアントから制御できること、つまりアカウントに所有されていることを確認する。
    * クライアントは、認証状態を確認する URL から課題を取得し、課題への対応の準備ができたらサーバに通知する。サーバは課題への対応を確認し、識別子の承認を行う。クライアントは承認が行われるまで認証状態をポーリングしながら待つ。
    * 課題の方式は現在 HTTP 検証、DNS 検証の2つがある。これについては後述する。
4. 証明書の発行
    * このステップでは、承認済みの識別子に対して証明書を発行する。
    * クライアントは、証明書発行手続き用の URL から CSR をサーバに送り、サーバはその識別子が承認済みであること、CSR の内容が許容可能であることを確認し、証明書ダウンロード用 URL を返す。

証明書の更新時はアカウント作成を除くステップを繰り返す。これにより、クライアント-サーバ間の HTTPS でのやり取りのみで CA から証明書を受け取ることができる。

さて、後回しにした識別子の認証だが、ACME では現状認証方式としてドメイン認証のみ対応している。ただ、プロトコル自体は組織認証、拡張認証などにも対応できるよう設計されており、将来的にはそれらのサポートもされるかもしれない。ドメイン認証は、HTTP による検証と DNS による検証の2通りに対応している。HTTP 検証では、課題として受け取ったトークンとアカウントに紐づく鍵のフィンガープリントを ``http://<domain>/.well-known/acme-challenge/<token>`` にデプロイできることにより、そのドメインがアカウントに所有されているとみなす。DNS 検証では、 ``_acme-challenge`` サブドメイン、つまり ``_acme-challenge.<domain>`` の TXT レコードに、トークンとアカウントに紐づく鍵のフィンガープリントをデプロイできることにより、そのドメインがアカウントに所有されているとみなす。この部分のデプロイをクライアントが自動化できれば、検証も自動化することができる。

Caddy で CA を立てる
----------------------------

さて、ここまでで X.509 PKI における CA の役割、ACME による CA と末端エンティティとのやり取りの自動化について見てきた。では、実際に世の中でどう運用されているかだが、まず ACME クライアントの有名どころが `Certbot <https://github.com/certbot/certbot>`_ で、ACME サーバとのやり取りと CSR の生成はもちろん、HTTP 検証に必要な項目を Web サーバにデプロイするところまでこなしてくれたり、有効期限によって証明書の再発行を制御する機構なども入っており、結構便利。Let's Encrypt 利用時はお馴染みという感じだろう。Certbot は接続先の ACME サーバとして Let's Encrypt をデフォルトで使うが、接続先をカスタマイズすることもできる。なので手元で ACME サーバを立てて、証明書を ACME 経由で作ることもできる。ACME 対応の CA は Let's Encrypt が自身が使っている `Boulder <https://github.com/letsencrypt/boulder>`_ という CA の実装を公開している。他にも `smallstep CA <https://smallstep.com/docs/step-ca/#introduction-to-step-ca>`_ というミニマルな実装がある。CA としての機能は欠けてるものも多いが最低限のものは揃ってる。今回紹介する `Caddy <https://caddyserver.com/>`_ はバックエンドで `smallstep CA`_ を使っている。

さて、今回紹介する Private CA は、Certbot をクライアントに使って、ACME サーバは `Caddy`_ という Web サーバで立てる方法だ。 `Caddy`_ を使う場合、OCSP レスポンダは諦める必要があるが、まあかなり小規模な常時 TLS 環境を手元で作りたいとかなら、割と手軽でそこまで困らないかなという感じ。 `Caddy`_ は割と面白い機能を持った Web サーバで、ACME クライアントの機能を備えて自動で証明書を取得したり更新してくれたり、Private CA を立ててそこから証明書を自動で発行して使ったりしてくれる。さらに、Private CA に対して ACME サーバを立てる機能も持っている。今回はこの機能を使っていく。

caddy のインストールは、https://caddyserver.com/docs/install に書いてある手順でできる。Debian なら、

::

    $ sudo apt install -y debian-keyring debian-archive-keyring apt-transport-https
    $ curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/gpg.key' | sudo gpg --dearmor -o /usr/share/keyrings/caddy-stable-archive-keyring.gpg
    $ curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/debian.deb.txt' | sudo tee /etc/apt/sources.list.d/caddy-stable.list
    $ sudo apt update
    $ sudo apt install caddy

の手順でリポジトリ追加してパッケージインストール可能。結構お手軽。Docker image もである: https://hub.docker.com/_/caddy

設定ファイルは ``/etc/caddy/Caddyfile`` にある。ドキュメントは https://caddyserver.com/docs/caddyfile。入れておくといい設定は、以下::

    {
        auto_https disable_redirects
        skip_install_trust
    }

1つ目は勝手に HTTP サーバが立つのを無効化する設定。Caddy はデフォルトで全てのサイトを HTTPS でサービングし、HTTP には HTTPS へのリダイレクトサーバを立てる。ドメインを指定する場合 Let's Encrypt から証明書を発行、IP アドレスを指定する場合自己署名証明書を発行する。ただ、勝手に 80 ポート占有されるのは微妙なので、リダイレクトの方は無効化しておく。2つ目は、Private CA のルート証明書を Java のトラストアンカーや ca-certificates に加えてくるおせっかい機能を止める。

Private CA の設定は、pki オプションで指定できる::

    {
        pki {
            ca local {
                root_cn "My Local CA - 2023 ECC Root"

                intermediate_lifetime 30d

                root {
                    cert /etc/pki/caddy/ca/local/root.crt
                    key /etc/pki/caddy/ca/local/root.key
                }
            }
        }
    }

``root_cn`` オプションはルート証明書の共通名を指定する。 ``intermediate_lifetime`` は中間証明書の有効期間を 30 日に伸ばす。デフォルトは 7 日。ま、基本デフォルトの期間から変えなくていいと思う。root.cert / root.key はルート証明書とその秘密鍵のファイルを指定している。指定しない場合は自動生成してくれる。証明書の生成は、以下の要領でできる::

    $ cat > cacert.cnf
    [ req ]
    distinguished_name  = req_distinguished_name
    req_extensions      = req_ext
    default_md          = sha512

    [ req_distinguished_name ]
    commonName                      = Common Name
    commonName_default              = My Local CA - 2023 ECC Root

    [ req_ext ]
    subjectKeyIdentifier    = hash
    authorityKeyIdentifier  = keyid:always, issuer:always
    basicConstraints        = critical, CA:TRUE, pathlen:1
    keyUsage                = critical, cRLSign, keyCertSign
    $ openssl req -config cacert.cnf \
        -x509 \
        -newkey EC \
        -pkeyopt ec_paramgen_curve:secp384r1 \
        -pkeyopt ec_param_enc:named_curve \
        -days 3600 \
        -out /etc/pki/caddy/ca/local/root.crt \
        -passout 'pass:'
        -keyout /etc/pki/caddy/ca/local/root.key

これによりルート証明書を永続化しつつ、他のサーバに配布してトラストアンカーに入れておけば、Private CA から発行された証明書で HTTPS サーバを立てて通信できる。Caddy で ACME サーバを立てるには、以下の設定を ``Caddyfile`` に追加する::

    acme.mylocal.domain:6100 {
        tls internal

        acme_server {
            ca local
            lifetime 10d
        }
    }

これにより、6100 ポートからの acme.mylocal.domain ホストへのリクエストは ACME サーバで受けるようにできる。 ``tls internal`` は Private CA から発行された証明書を使う設定になる。この ACME サーバは Private CA から10日有効の証明書を発行する。デフォルトは 12 時間。流石に 12 時間は短いと思うが、基本 2 日以内にはしとく方がいいだろう。

この ACME サーバから nginx 向けに Certbot を使って証明書を発行してみる。まず、Certbot と Nginx をインストールする。Debian なら、

::

    apt install -y nginx certbot

で入る。まず、Nginx の方で、Certbot の Web ルートをサービングする HTTP サーバを立てる:

.. code-block:: nginx

    server {
        listen 80;
        listen [::]:80;

        server_name _;

        location /.well-known/acme-challenge/ {
            allow all;
            root /var/www/certbot/;
            default_type "text/plain";
            try_files $uri =404;
        }

        location / {
            return 301 https://$host$request_uri;
        }
    }

次に certbot で証明書を発行する::

    env REQUESTS_CA_BUNDLE=/etc/pki/caddy/ca/local/root.crt \
        certbot certonly
        --server https://acme.mylocal.domain:6100/acme/local/directory
        --webroot --webroot-path /var/www/certbot
        --domain www.mylocal.domain
        --email contact@myemail.domain
        --agree-tos
        --no-eff-email

``REQUESTS_CA_BUNDLE`` は Python の環境変数で、トラストアンカーとなるルート証明書のバンドルファイルのパスを指定できる。今回は Private CA のルート証明書を指定している。 ``--server`` には、ACME サーバのディレクトリパスを指定する。デフォルトは、Let's Encrypt の https://acme-v02.api.letsencrypt.org/directory が使われているが、今回は Caddy の ACME サーバの URL を使っている。Caddy の場合、ルートパスは、 ``/acme/<CA name>/directory`` になる。webroot はトークンファイルをデプロイして HTTP サーバにそれをサーブしてもらい、HTTP 検証で課題をこなすモードで、 ``/var/www/cerbot`` にトークンファイルが置かれるのでそれを Nginx が ACME サーバに返すことで識別子の承認を得る。 ``agree-tos`` はサービス利用規約に同意するフラグで、 ``--no-eff-mail`` は指定したメールアドレスで電子フロンティア財団からのニュースを購読しないフラグ。これで、ドメイン設定などをちゃんとしていれば、 ``/etc/letsencrypt/live/www.mylocal.domain`` に証明書と秘密鍵などが発行される。

後は、nginx 側で HTTPS サーバに証明書を設定すればよい:

.. code-block:: nginx

    server {
        listen 443 ssl http2;
        listen [::]:443 ssl http2;

        server_name www.mylocal.domain;

        ssl_certificate         /etc/letsencrypt/live/www.mylocal.domain/fullchain.pem;
        ssl_certificate_key     /etc/letsencrypt/live/www.mylocal.domain/privkey.pem;
        ssl_trusted_certificate /etc/letsencrypt/live/www.mylocal.domain/chain.pem;

        add_header Strict-Transport-Security  "max-age=63072000; includeSubDomains" always;
        add_header X-Frame-Options            "SAMEORIGIN"                          always;
        add_header X-Content-Type-Options     "nosniff"                             always;
        add_header X-XSS-Protection           "1; mode=block"                       always;

        location / {
            return 200 "OK";
        }
    }

証明書は定期的に更新する必要がある。特に ACME では、自動で管理できる分発行される証明書の有効期間が短くしてあることが多い。Private CA なら尚更、管理をルーズにする分有効期間は短くしておく方がいいだろう。さて、有効期間を短くする場合、手動での更新とかやってられないので、証明書更新の自動化が必要になってくるだろう。Debian の Certbot パッケージはデフォルトで weekly で全部の証明書更新をかける timer を入れてくれる。ただ、今回は weekly じゃ追いつかないのとルート証明書の指定が必要なので、自分で timer を書くのが良いだろう。まず、systemd oneshot service を以下の感じで書く:

.. code-block:: ini

    [Unit]
    Description=certbot renew service for www.mylocal.domain

    [Service]
    Type=oneshot
    Environment=REQUESTS_CA_BUNDLE=/etc/pki/caddy/ca/local/root.crt
    ExecStart=certbot renew \
        --cert-name www.mylocal.domain \
        --post-hook 'systemctl reload nginx'

これを ``/etc/systemd/system/certbot_renew_www.mylocal.domain.service`` におく。後はこの service を呼び出す systemd timer を以下の感じで書く:

.. code-block:: ini

    [Unit]
    Description=certbot renew timer for www.mylocal.domain
    ConditionPathExists=/etc/letsencrypt/live/www.mylocal.domain/fullchain.pem

    [Timer]
    OnCalendar=daily
    Persistent=true

    [Install]
    WantedBy=timers.target

これを ``/etc/systemd/system/certbot_renew_www.mylocal.domain.timer`` におく。後は、 ``systemctl start certbot_renew_www.mylocal.domain.timer`` で timer を開始しておけば、daily で更新が走るようになる。後はルート証明書をブラウザにインストールするなりしておけば良い。

これで、プライベートネットワーク内、プライベートアドレスについても手軽に証明書発行して TLS 対応できる。CRL 配布対応がないのが残念だが、小規模なオレオレ環境なら特に困らないだろう。

まとめ
-------------

今回は X.509 PKI の仕組みとその管理を自動化する ACME プロトコルについて紹介した。また、Caddy という Web サーバで、Private CA の ACME サーバを立て、証明書を自動発行し、プライベートネットワーク内で TLS 環境を構築する方法を紹介した。これによりプライベートネットワーク内であっても TLS 網が結構手軽に構築できるし、Let's Encrypt の検証環境を手元で構築したりもできる。

TLS プロトコル自体の説明は巷に結構あるのだが、X.509 PKI の情報となるとかなり限られるし、毎回調べることになってたので書いた。X.509 の拡張とかなんとなく指定してるものも多かったので、割と情報整理になってよかった。後、ACME もなんとなくの部分が多かったので、知識補強になったかなという感じ。ACME もうちょっと流行ってほしい。2020 年過ぎても証明書の手動更新してるのは流石になあって感じがある。まあ、TLS 必須と謳われてる割には CA/B は迷走してる感じもあるが。

では、今回はこれで。

.. [#rfc-5280] https://datatracker.ietf.org/doc/html/rfc5280
.. [#rfc-6125-sec6.4.4] https://datatracker.ietf.org/doc/html/rfc6125#section-6.4.4
.. [#cabforum-servercert] https://github.com/cabforum/servercert
