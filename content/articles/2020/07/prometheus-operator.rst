Prometheus Operator で k8s を監視する
=====================================

:tags: Prometheus, Grafana, Kubernetes, Docker
:category: 運用

Prometheus は、メトリクスの監視を行うためのモニタリングツールだ。各種メトリクスを出す exporter とそれを集計する Prometheus サーバ、及びその集計結果を監視しアラートを出すアラートマネージャで主に構成されている。Prometheus それ自体は便利なんだが、Kubernetes クラスタを監視したい場合色々追加で設定が必要で、結構めんどくさい。そこで、Kubernetes 用に Prometheus をカスタマイズして提供してくれるパッケージがいくつか出ている。今回はそのうちの Prometheus Operator を触る機会があって、せっかくなので使い方を備忘録として残しておく。

Prometheus Operator のインストール
----------------------------------

まずは、Kubernetes / helm が入った環境用意する。一応、Kubernetes のバックエンドは今回は Docker を想定する。Kubernetes 自体は Windows / macOS なら Docker Desktop で付属としてついてくるのでそれを使うといいだろう。WSL2 の環境なら、

1. Docker Desktop をインストール
2. 設定から「Kubernetes」->「Enable Kubernetes」
3. 「Resources」->「WSL Integration」-> 普段使ってる WSL コンテナを有効化

すれば、WSL コンテナ内で ``docker`` / ``kubectl`` などの CLI が使えるようになる。Helm は、大体各種パッケージマネージャでインストールできる。Linuxbrew とか使って、

::

    brew install helm

とかすれば入る。後は、

::

    helm repo add stable https://kubernetes-charts.storage.googleapis.com/

とかで標準のチャートリポジトリを追加しておく。後、Kubernetes Dashboard もデプロイしておくとよい。こっちも helm で入れられる::

    helm repo add kubernetes-dashboard https://kubernetes.github.io/dashboard/
    kubectl create namespace kubernetes-dashboard
    helm install kubernetes-dashboard kubernetes-dashboard/kubernetes-dashboard \
        --namespace kubernetes-dashboard \
        --set protocolHttp=true,service.externalPort=80

Prometheus Operator はかなり色々入るので名前空間を分けておいた方がいい。今回は ``prometheus`` という名前空間を作って、そこにインストールしていく。``helm`` で簡単に入るので、それでインストールする::

    kubectl create namespace prometheus
    helm install prometheus stable/prometheus-operator --namespace prometheus

ポッドの様子を見てみる::

    $ kubectl -n prometheus get pods
    NAME                                                     READY   STATUS              RESTARTS   AGE
    alertmanager-prometheus-prometheus-oper-alertmanager-0   2/2     Running   0          77s
    prometheus-grafana-8c6966f8f-m45xx                       2/2     Running   0          99s
    prometheus-kube-state-metrics-6df5d44568-p464c           1/1     Running   0          99s
    prometheus-prometheus-node-exporter-2r7vq                1/1     Running   0          99s
    prometheus-prometheus-oper-operator-749cd475f6-j7ftd     2/2     Running   0          99s
    prometheus-prometheus-prometheus-oper-prometheus-0       3/3     Running   1          68s

サービスも見てみると、

::

    $ kubectl -n prometheus get svc
    NAME                                      TYPE        CLUSTER-IP      EXTERNAL-IP   PORT(S)                      AGE
    alertmanager-operated                     ClusterIP   None            <none>        9093/TCP,9094/TCP,9094/UDP   2m33s
    prometheus-grafana                        ClusterIP   10.111.67.1     <none>        80/TCP                       2m55s
    prometheus-kube-state-metrics             ClusterIP   10.97.237.255   <none>        8080/TCP                     2m55s
    prometheus-operated                       ClusterIP   None            <none>        9090/TCP                     2m24s
    prometheus-prometheus-node-exporter       ClusterIP   10.100.44.54    <none>        9100/TCP                     2m55s
    prometheus-prometheus-oper-alertmanager   ClusterIP   10.98.127.253   <none>        9093/TCP                     2m55s
    prometheus-prometheus-oper-operator       ClusterIP   10.97.208.129   <none>        8080/TCP,443/TCP             2m55s
    prometheus-prometheus-oper-prometheus     ClusterIP   10.98.54.131    <none>        9090/TCP                     2m55s

みたいな感じになる。おおむねそれぞれのサービスは

``prometheus-oper-prometheus``
    Prometheus 本体

``prometheus-oper-alermanager``
    Alert Manager 本体

``prometheus-node-exporter``
    ノードごとのメトリクス出力用 node exporter

``kube-state-metrics``
    Kubernetes のオブジェクトごとのメトリクス出力用 exporter

``grafana``
    Prometheus のメトリクス可視化用の Grafana 本体

``prometheus-oper-operator``
    Prometheus Operator 本体

みたいになっている。Prometheus Operator の主な役割は、カスタムリソースによる Prometheus / AlertManager の設定管理及び監視対象との接続になる。具体的にどのように設定を管理していくは、後で見ていくとして、とりあえずデプロイされたものを触ってみる。まず、Prometheus 本体を見てみる。``localhost`` で見たい場合は、プロキシ立てると良い::

    kubectl proxy

で、

::

    http://localhost:8001/api/v1/namespaces/prometheus/services/http:prometheus-prometheus-oper-prometheus:web/proxy/
  
にアクセスするといいだろう。Prometheus の画面が表示されて、色々ターゲットが追加されてるのが分かると思う。お試し環境だと etcd とかデプロイしてないと思うので、etcd が立ち上がってないとかアラートが出てると思うが、それらのアラートの調整の仕方は後で紹介する。``Grafana`` の画面も見てみるといいだろう。``Grafana`` の方は proxy から見るのはちょっときついので、ポートフォワードして見るといい::

    kubectl port-forward service/prometheus-grafana 3000:80 --address 0.0.0.0 -n prometheus

Grafana の方はログインが必要で、ユーザ ``admin`` / パスワード ``prom-operator`` で入れる。このパラメータは、`チャートのパラメータ <https://github.com/helm/charts/tree/master/stable/prometheus-operator#grafana>`_ として弄れるようになっている。Grafana も初期段階で色々テンプレートが追加されていて便利という感じ。

カスタムリソース
----------------

これだけ見ると、まあ単に初期設定モリモリの Prometheus 周辺ツールがパッとデプロイできるだけとなるんだけど、Prometheus Operator の神髄はここから。Prometheus Operator は、Prometheus の設定をカスタムリソースとしてデプロイできる。まずは、どういうリソースがデプロイされているかを見てみる。以下の URL から Dashboard にアクセスする::

    http://localhost:8001/api/v1/namespaces/kubernetes-dashboard/services/http:kubernetes-dashboard:http/proxy/

そして、上のメニューから ``prometheus`` ネームスペースを選択し、左のメニューの「カスタムリソース定義」欄を選択する。すると、幾つかの項目が表示されると思う。それぞれ、

Prometheus
    Prometheus ポッドのテンプレート。このテンプレートを基に operator が Prometheus ポッドをデプロイする。

Alertmanager
    Alertmanager ポッドのテンプレート。このテンプレートを基に operator が Alertmanager ポッドをデプロイする。

PrometheusRule
    Prometheus のルールを表すカスタムリソース

ThanosRuler
    Thanos のルールを表すカスタムリソース (Thanos 触ったことないので、今回は割愛)

みたいなもの。後、``PodMonitor`` / ``ServiceMonitor`` があるが、これは後述するので今は割愛する。Prometheus のルールを弄りたかったら PrometheusRule カスタムリソースをいじることになる。新規にルールを追加したかったら新しく PrometheusRule を作ってデプロイすればいいし、既存のものをいじりたかったら既存のカスタムリソースを弄ればいい。

試しに、etcd のアラートを消してみる。PrometheusRule カスタムリソースの欄に ``prometheus-prometheus-oper-etcd`` というカスタムリソースがある。定義を見てみると、``etcdMembersDown`` などのアラートルールが記載されてるのが分かると思う。このカスタムリソースでは etcd ターゲットに関するルールが規定されている。これを削除してみる::

    kubectl -n prometheus delete prometheusrules prometheus-prometheus-oper-etcd

で、しばらく待ってから [#watch-completed-on-log]_ Prometheus のダッシュボードをのぞいてみると、アラートから etcd 関連のルールが消えてることが確認できると思う。次は新しくルールを追加してみる。以下のカスタムリソースを作成する:

.. code-block:: yaml

    apiVersion: monitoring.coreos.com/v1
    kind: PrometheusRule
    metadata:
        name: mysample-alert.rules
        namespace: prometheus
        labels:
            app: prometheus-operator
            release: prometheus
    spec:
        groups:
        -   
            name: mysample-alert.rules
            rules:
            -
                alert: MyTargetDown
                expr: >-
                    100 * (count(up == 0) BY (job, namespace, service) / count(up) BY
                    (job, namespace, service)) > 10
                for: 10m
                labels:
                    severity: warning
                annotations:
                    message: >-
                        {{ $labels.job }}/{{ $labels.service }} targets in
                        {{ $labels.namespace }} namespace are down.

これを、例えば ``mysample-alert.rules.yaml`` として、

::

    kubectl apply -f mysample-alert.rules.yaml

とやると、デプロイできる。後はしばらく待てば、Prometheus Dashboard のアラート一覧に、``MyTargetDown`` というアラートが追加されてることが分かる。このようにして新しいルールを追加したり、既存のルールを編集・削除したりできる。

ところで、上でデプロイしたルールの ``labels`` で設定されてる ``app`` / ``release`` の値は、デフォルトでデプロイされている prometheus でセレクタとして設定されてる値で、これを設定しないとルールが認識されないようになっている。実際にその部分を見てみる。Prometheus カスタムリソースとして、1つだけデプロイされている ``prometheus-prometheus-oper-prometheus`` の設定をのぞいてみる。ここでは、

* 接続する Alertmanager
* ServiceMonitor / PodMonitor のセレクタ
* PrometheusRule のセレクタ

や、Prometheus ポッドの設定ができる。PrometheusRule のセレクタは、

.. code-block:: yaml

    ruleNamespaceSelector: {}
    ruleSelector:
        matchLabels:
            app: prometheus-operator
            release: prometheus

の部分がそうで、デフォルトの Prometheus ポッドでは ``app`` が ``prometheus-operator`` で、``release`` が ``prometheus`` のラベルを持っている PrometheusRule のみ認識され、Prometheus にルールとして取り込まれる。メトリクスによって監視サーバを分けたい場合などは、対応するルールにラベルを設定しておき、そのラベルに対するセレクタを設定した Prometheus カスタムリソースをデプロイすればいい。

Prometheus カスタムリソースから作られたポッドは、Prometheus のログがそのままログとして出ている。カスタムリソースで書いたルールが間違っていたら、ログにその旨が出力されるので、カスタムリソースがちゃんと設定されてるのにルールが反映されていない場合は確認してみるといい。カスタムリソースから作られた設定及びルールは、

* ``/etc/prometheus/config_out/prometheus.env.yaml``
* ``/etc/prometheus/rules/<pod名>/<rule名>.yaml``

に入っている。ログでは正常に動いてそうなのに、ルールが反映されない場合次に確認するのが、カスタムリソースに対応するルールファイルがあるかとそのファイルの中身がちゃんと入っているかだ。大体それを確認すればトラブルシューティングができるはず。

Alertmanager カスタムリソースはちょっとめんどいので今回は省略する。`ドキュメント <https://github.com/coreos/prometheus-operator/blob/master/Documentation/user-guides/alerting.md>`_ を参照のこと。基本的には、Alertmanager カスタムリソースを作成し、対応する alertmanager の設定を secret で作成する。で、Prometheus カスタムリソースにその Alertmanager カスタムリソースと secret を指定するとアラートが設定できる。

``PodMonitor`` / ``ServiceMonitor``
-----------------------------------

さて、ルールの変更とポッドの設定はできたが、Prometheus の監視対象、つまりターゲットの変更はどうすればいいのだろう？ そのターゲットの管理を担うのが ``PodMonitor`` / ``ServiceMonitor`` になる。それぞれ名前の通り、pod / service ごとのターゲットの設定ができる。

初期で追加されているターゲットは、それぞれ ServiceMonitor で設定されている。例えば、grafana の設定は、``prometheus-prometheus-oper-grafana`` という名前のカスタムリソースがそれに当たる。中身は、

.. code-block:: yaml

    spec:
        endpoints:
        -
            path: /metrics
            port: service
        namespaceSelector:
            matchNames:
            - prometheus
        selector:
            matchLabels:
                app.kubernetes.io/instance: prometheus
                app.kubernetes.io/name: grafana

のような内容になっている。``namespaceSelector`` で ``prometheus`` 名前空間、``selector`` でその名前空間のラベルでマッチするサービスを探すことになる。このセレクタに引っかかる service が、``prometheus-grafana`` になる。その内容は、

.. code-block:: yaml

    spec:
        type: ClusterIP
        ports:
        -
            name: service
            protocol: TCP
            port: 80
            targetPort: 3000
        selector:
            app.kubernetes.io/instance: prometheus
            app.kubernetes.io/name: grafana

となっている。ServiceMonitor は、このサービスの ``service`` という名前のポート、つまり 3000 番の ``/metrics`` パスをターゲットとして設定する。このように ``ServiceMonitor`` をデプロイしたり、編集することで、サービスに対してのターゲットを設定できる。試しに、etcd / proxy のターゲットがお試し環境だと機能していないので消してみる。それぞれ ``prometheus-prometheus-oper-kube-etcd`` / ``prometheus-prometheus-oper-kube-proxy`` という ServiceMonitor が対応している。これら2つを削除し、しばらく待つと、ターゲットからその2つが消えてることが確認できる。もちろん、自分でデプロイした exporter を持つサービスに対して自由にターゲットを設定できる。試しにやってみる。Kotlin で Spring Actuator を使ったメトリクス出力機能付き Web アプリを service としてデプロイし、その service に対して ServiceMonitor でメトリクスをターゲットとして追加してみる。

デプロイするサンプルアプリは、https://github.com/mizunashi-mana/kotlin-spring-actuator-sample 。k8s のリソースも書いてあるので、それでまずデプロイをする::

    git clone https://github.com/mizunashi-mana/kotlin-spring-actuator-sample
    cd kotlin-spring-actuator-sample
    ./gradlew docker
    kubectl apply -f k8s/deployment.yaml
    kubectl apply -f k8s/service.yaml

これで、actuator のメトリクスが出されるようになる。そのメトリクスの内容は、

::

    http://localhost:8001/api/v1/namespaces/default/services/http:spring-actuator-demo:web/proxy/actuator/prometheus

とかで見れる。これを ServiceMonitor で監視してみる。以下のようなリソースを書く:

.. code-block:: yaml

    apiVersion: monitoring.coreos.com/v1
    kind: ServiceMonitor

    metadata:
        name: spring-actuator-demo-monitor
        namespace: prometheus
        labels:
            release: prometheus

    spec:
        namespaceSelector:
            any: true

        selector:
            matchLabels:
                app: spring-actuator-demo
        
        endpoints:
        -
            port: web
            path: /actuator/prometheus

で、例えば ``demo-service-monitor.yaml`` として、デプロイする::

    kubectl apply -f demo-service-monitor.yaml

で、しばらく待ってから Prometheus のターゲットを確認すると、``spring-actuator-demo`` のためのターゲットが確認できると思う。実際にメトリクスが取れてるかは、Grafana にポートフォワードして、

::

    http://localhost:3000/explore?orgId=1&left=%5B%22now-1h%22,%22now%22,%22Prometheus%22,%7B%22expr%22:%22jvm_buffer_memory_used_bytes%22%7D,%7B%22mode%22:%22Metrics%22%7D,%7B%22ui%22:%5Btrue,true,true,%22none%22%5D%7D%5D

とかで確認してみるといいだろう。デフォルトでデプロイされてる Prometheus カスタムリソースは、``release: prometheus`` ラベルで PodMonitor / ServiceMonitor を探すので、そのラベルは設定する必要がある。それから、ターゲットのジョブラベルは明示的に設定することもできるが、指定しない場合は ``name`` が設定される。Prometheus では同じジョブを持つメトリクスを複数登録できないので、monitor の ``name`` は注意して設定しておくといいだろう。

今回のは service ごとにセレクトしてターゲットに設定をするが、pod ごとにセレクトできるのが PodMonitor になる。こちらは使い方は ServiceMonitor と同じで、pod のセレクトか service のセレクトかだけが異なるので、今回は省略する。

まとめ
------

というわけで、Prometheus Operator の使い方を紹介した。Prometheus Operator は Kubernetes 上での監視体制を整えるためのパッケージで、カスタムリソースで監視対象を追加したりルールを追加したりできる。監視対象の追加は Kubernetes のエコシステムに合わせた形で定義できるため、結構便利。ただ、現状は beta 版なのでこの先大きく使い方が変わる可能性がある。そこは注意した方がいいだろう。今回はそういう感じで。

.. [#watch-completed-on-log] 反映の完了は各 prometheus ポッドのログから確認できる。``Completed loading of configuration file`` というログが出たら反映が行われている。 
