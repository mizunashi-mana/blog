React の View / State 分離パターン
==========================================

:tags: React, アーキテクチャ, Web, 状態管理, MVVM
:category: プログラミング

React は昨今の Web アプリ開発において当たり前に使われるようになり、エコシステムも発展し続けている。そして、Web における DOM / JavaScript とどう付き合いつつ、モバイルアプリでは当たり前だった、フロントエンドとバックエンドがそれぞれ独立のライフサイクルを持ちつつ API 通信によってのみ繋がる世界を実現するかにおいて、一つの解となっている。一方で、動きのあるフロントエンドにおける永遠の課題が、ビューと状態管理をどう保守するかであり、React コミュニティも例に漏れず非常に苦心していて、さまざまな手法が提案されては立ち消えている。

さて、ビューと状態への付き合い方について僕もよく悩むわけだが、今回は僕がよく使っている独自のデザインパターンについて備忘録がてら紹介したいと思う。

ビューと状態管理
------------------------

バックエンド開発では、ドメインロジックやデータ永続化が主たる関心事になるため、それらをどう認知負荷を抑えつつ変更容易性を保って管理するかが主眼となり、クリーンアーキテクチャなどのアーキテクチャパターンが幾つか提唱されている。一方、フロントエンド開発では大きく事情が異なる。フロントエンド開発において、アーキテクチャの関心事は大きく「ビュー」と「状態管理」の2つに集約される。というのは、ドメインロジックなどはセキュリティとフロントエンドの複数化などの観点からバックエンドに集約させておくのが良いため、逆説的にフロントエンドではドメインロジック自体はバックエンド API に委ねられ、代わりにユーザーの操作に対してリアルタイムに応答するUIの構築が中心となるからだ。結果として、フロントエンドのアーキテクチャはビューの描画と、それを駆動する状態の管理が主体となる。

ビューと状態管理において、重要なポイントは2つある。1つはビューと状態管理の分離、もう1つは情報の流れの方向性だ。

ビューと状態管理は、それぞれ異なる関心事を持っている。ビューは「何をどう描画するか」「どのイベントを受け付けるか」に関心があり、状態管理は「どのようなデータをどのような形式で保持するか」「ある操作に対して状態をどう遷移させるか」に関心がある。これらが分離されていないと、例えばUIのレイアウトに引っ張られて状態の保持形式を変える必要が生じ、状態が必要以上に肥大化して更新処理も複雑化したり、状態遷移のロジックを修正したいだけなのにJSXの構造を読み解く必要が出てきたりする。ビューと状態管理を分離し、両者の間に明確なインターフェースを設けることで、それぞれを独立して理解・変更・テストできるようになる。特に状態のプラクティスは必要最小限の情報を保持することにあるが、ビューと状態管理ロジックが密室に結合しているとこの原則が破られやすく、ビューの描画のための付随情報が状態に載ってしまうといった事が起こりやすい。これを防ぐには、ビューと状態管理のロジックを分離しつつ、それぞれの接続を行いやすくする工夫が重要となる。

また、ビューはユーザーのインタラクションに応じてイベントを発火し、イベントは状態を更新し、状態の変化はビューの再描画を引き起こすため、互いに作用し合う。この相互作用自体は避けられないものだが、情報が双方向に流れる設計になっていると、ある状態の変化がどこから引き起こされたのか、あるイベントが最終的にどの状態をどう変えるのかを追跡するのが困難になる。特にコンポーネントが成長するにつれ、イベントハンドラから直接状態を書き換えたり、描画の途中で副作用的に状態を更新したりといったショートカットが入り込みやすく、データの流れが絡み合っていく。これに対して、ビューのイベント発火 → 状態の更新 → 描画パラメータの算出 → ビューの再描画という流れを常に一方向に保つことで、データの因果関係を追跡しやすくなり、各段階の責務も明確になる。

React アーキテクチャの歴史
------------------------------

ビューと状態管理の分離と単方向データフローは React や Web 固有の話題ではなく、UI 開発で一般に研究されてきた分野で、MVVM やリアクティブプログラミングなど様々なアイデアが登場してきた。React フロントエンドアーキテクチャの分野でもそれらの研究の流れを汲みながら、React 登場時から活発に研究されてきた。

Facebook が React と共に2014年に提案した `Flux アーキテクチャ <https://facebookarchive.github.io/flux/>`_ は、Action → Dispatcher → Store → View の単方向データフローを規定した。これにより、状態がどこで変更されるのかを予測可能にするという指針が React コミュニティに根付いた。

その思想を洗練させた `Redux <https://redux.js.org/>`_ は、 `The Elm Architecture（TEA） <https://guide.elm-lang.org/architecture/>`_ に着想を得て、単一ストアと Action / Reducer による状態管理を実現した。Redux は Action / Reducer / Store / Selector という責務分割を持ち、状態遷移を純粋関数として定義するという強力なモデルを提供した。一方で、アプリケーション全体の状態を一つのストアで管理する前提のため、ボイラープレートの多さやコンポーネントローカルな状態管理との棲み分けが課題となっていた。

Dan Abramov が2015年に提唱した `Container / Presentational パターン <https://medium.com/@dan_abramov/smart-and-dumb-components-7ca2f9a7c7d0>`_ は、コンポーネントを2つの役割に分離するアプローチだった。Container コンポーネントがデータ取得と状態管理を担い、Presentational コンポーネントが props を受け取って描画のみを行う。このパターンの本質的な利点は、ビューのインターフェースを props として明示することにあった。Presentational コンポーネントの props 型定義を見れば、そのビューが何を必要としているかが一目で分かる。また、状態管理ロジックをビューから切り離し、さらに再利用可能にするアイデアとして Higher-Order Components（HOC）や Render Props といったデザインパターンも登場した。HOC はコンポーネントを受け取って状態やハンドラを props として注入した新しいコンポーネントを返す関数であり、Redux の ``connect()`` が代表例だ。Render Props は状態やハンドラを引数に取る関数を props として受け取り、その戻り値を描画するパターンで、ロジックの提供側が描画方法を利用側に委ねることができた。いずれも状態管理ロジックを複数のコンポーネント間で共有しやすくなるという利点があった。

ただこれらはいずれも記述量とパフォーマンスのオーバーヘッドの問題に悩まされていた。React 16 で導入された `Hooks <https://react.dev/blog/2023/03/16/introducing-react-dev#going-all-in-on-modern-react-with-hooks>`_ は、これらの課題に対する転換点となった。今や当たり前となった ``useCallback`` や ``useMemo`` によってイベントハンドラや派生データの最適化が可能になり、``useReducer`` によって外部ライブラリを必要とせず状態遷移を表現できるようになった。そして何より、カスタムフックによってビュー以外のロジックをコンポーネントの分割なしに切り出せるようになった。Hooks 以降も、 `Headless Components <https://www.merrickchristensen.com/articles/headless-user-interface-components/>`_ によるロジックと見た目の分離を再利用目的で追求するパターンや、 `XState <https://xstate.js.org/>`_ のような状態機械ベースのアプローチ、 `Zustand <https://zustand.docs.pmnd.rs/>`_ や `Jotai <https://jotai.org/>`_ のような軽量なストアライブラリによるコンポーネント横断の状態共有など、ビューと状態の分離、状態管理に関するさまざまな手法が発展し続けている。

これらの歴史を踏まえると、各手法は「ビューと状態の分離」「情報の一方向フロー」「責務の明確化」という共通の課題に取り組んできたことが分かる。一方で、これらのアプローチはアプリケーショングローバルな状態管理をどうするかという話と、ビューとそれ以外のロジックをどう分離するかという2つの別軸で議論が動いており、単一コンポーネントレベルでビューと状態管理をどう分離しつつ、単方向のデータフローを実現するかのデザインパターンは僕はあまりいいのが見つけられてなかった。特にフロントエンドアプリケーションではアプリケーション全体でのライフサイクルで動く状態というのはむしろ少なく、コンポーネント単位でのライフサイクルを持つ状態の方が多い。一方で Redux や Zustand といったライブラリをそこに使ってしまうのは、依存をアプリケーション全体に広げてしまい、過剰に感じる事が多かった。

View / State 分離パターン
--------------------------------

そこで最近はグローバルなコンポーネントを超えた状態共有は Zustand などを併用しつつ、コンポーネント単位の細かい状態管理にはあえて Zustand などの状態管理を利用せず、React の標準 API を使った View / State 分離パターンを使って書く事が個人的には多くなってきた。View / State 分離パターンは、React コンポーネントを以下の4つのパーツで構築するデザインパターンだ:

``State`` （状態）
    状態の保存形式と状態遷移を定義する。 ``useReducer`` で記述する。

``Handler`` （ハンドラ）
    イベントをハンドリングし、状態に対する更新アクションを発行する。 ``useCallback`` で記述する。

``Selector`` （セレクタ）
    状態からビューの描画パラメータを計算する。必要に応じて ``useMemo`` を利用する。

``View`` （ビュー）
    ``ViewProps`` 型を受け取り、どの要素をどう描画し、どのイベントをハンドリングするかを定義する。JSX の本体。

State / Handler / Selector は ``useViewProps`` というカスタムフックにまとめ、コンポーネントは ``useViewProps`` の戻り値である ``ViewProps`` からビューを記述する。 ``useViewProps`` は MVVM における ViewModel に近い役割を果たしており、内部の状態をビューが消費可能な形に変換して提供する。これにより以下の単方向データフローを実現する::

    View（イベント発火）
      → Handler（dispatch）
        → State（状態更新）
          → Selector（派生データ計算）
            → ViewProps
              → View（再描画）

具体的なコード例を見てみよう。フィルタ付き Todo リストを題材にする。まず、State として状態の型と reducer を定義する。状態は Todo の配列と現在のフィルタモードだけを保持し、ビューでの使いやすさなどとは切り離して最小限の情報に留めるのが大事だ:

.. code-block:: typescript

    type Todo = {
        id: number;
        text: string;
        done: boolean;
    };

    type Filter = 'all' | 'active' | 'completed';
    const FILTERS: Filter[] = ['all', 'active', 'completed'];

    type State = {
        nextId: number;
        todos: Todo[];
        filter: Filter;
    };

    type Action =
        | { type: 'add'; text: string; }
        | { type: 'toggle'; id: number; }
        | { type: 'remove'; id: number; }
        | { type: 'setFilter'; filter: Filter }
        | { type: 'clearCompleted' };

    function reducer(state: State, action: Action): State {
        switch (action.type) {
            case 'add':
                return {
                    ...state,
                    nextId: state.nextId + 1,
                    todos: [
                        ...state.todos,
                        { id: state.nextId, text: action.text, done: false },
                    ],
                };
            case 'toggle':
                return {
                    ...state,
                    todos: state.todos.map((todo) =>
                        todo.id === action.id
                            ? { ...todo, done: !todo.done }
                            : todo,
                    ),
                };
            case 'remove':
                return {
                    ...state,
                    todos: state.todos.filter((todo) => todo.id !== action.id),
                };
            case 'setFilter':
                return { ...state, filter: action.filter };
            case 'clearCompleted':
                return {
                    ...state,
                    todos: state.todos.filter((todo) => !todo.done),
                };
        }
    }

次に ``ViewProps`` を定義する。ここがパターンの要だ。ビューが必要とする情報を型として明示する。状態の内部表現（ ``Todo[]`` と ``Filter`` ）がそのまま露出するのではなく、ビューにとって扱いやすい形に変換された値が並ぶ:

.. code-block:: typescript

    type Props = {
        initialFilter: Filter;
    };

    type ViewTodo = {
        id: number;
        text: string;
        done: boolean;
    };

    type ViewProps = {
        // Selector: 状態から導出されたビュー用データ
        filteredTodos: ViewTodo[];
        remainingCount: number;
        currentFilter: Filter;

        // Handler: ビューから発火されるイベント
        onAddTodo: (text: string) => void;
        onToggleTodo: (id: number) => void;
        onRemoveTodo: (id: number) => void;
        onChangeFilter: (filter: Filter) => void;
        onClearCompleted: () => void;
    };

そして ``useViewProps`` で State / Handler / Selector を統合する:

.. code-block:: typescript

    function useViewProps({ initialFilter }: Props): ViewProps {
        // --- State ---
        const [state, dispatch] = useReducer(reducer, {
            nextId: 0,
            todos: [],
            filter: initialFilter,
        });

        // --- Handler ---
        const onAddTodo = useCallback((text: string) => {
            const trimmed = text.trim();
            if (trimmed === '') {
                return;
            }

            dispatch({ type: 'add', text: trimmed });

            // 追加した Todo が見えるよう、フィルタを 'all' に戻す
            if (state.filter !== 'all') {
                dispatch({ type: 'setFilter', filter: 'all' });
            }
        }, [state.filter]);

        const onToggleTodo = useCallback((id: number) => {
            dispatch({ type: 'toggle', id });
        }, []);

        const onRemoveTodo = useCallback((id: number) => {
            dispatch({ type: 'remove', id });
        }, []);

        const onChangeFilter = useCallback((filter: Filter) => {
            dispatch({ type: 'setFilter', filter });
        }, []);

        const onClearCompleted = useCallback(() => {
            if (!state.todos.some((t) => t.done)) {
                return;
            }

            dispatch({ type: 'clearCompleted' });

            // 完了済みを消した後に空リストが表示されるのを防ぐ
            if (state.filter === 'completed') {
                dispatch({ type: 'setFilter', filter: 'all' });
            }
        }, [state.todos, state.filter]);

        // --- Selector ---
        const filteredTodos = useMemo(() => {
            switch (state.filter) {
                case 'all':
                    return state.todos;
                case 'active':
                    return state.todos.filter((t) => !t.done);
                case 'completed':
                    return state.todos.filter((t) => t.done);
            }
        }, [state.todos, state.filter]);

        const remainingCount = useMemo(
            () => state.todos.filter((t) => !t.done).length,
            [state.todos],
        );

        return {
            filteredTodos,
            remainingCount,
            currentFilter: state.filter,
            onAddTodo,
            onToggleTodo,
            onRemoveTodo,
            onChangeFilter,
            onClearCompleted,
        };
    }

今回の例では ``useViewProps`` の中で State、Handler、Selector を一緒に定義しているが、これらをファイルに分割して定義してもいいだろう。コンポーネントの大きさによってそこは調節でき、まさに責務によって分けて定義できるのがこのパターンの魅力となる。また、分けて定義する事でビューに依存せずロジックをテストすることもできるようになる。

最後に、コンポーネント本体は ``useViewProps`` の戻り値からビューを記述するだけになる。ビューは ``ViewProps`` の中身だけを知っていればよく、状態がどう保存されフィルタリングがどう計算されているかを意識する必要がない:

.. code-block:: tsx

    const TodoList = (props: Props) => {
        const {
            filteredTodos,
            remainingCount,
            currentFilter,
            onAddTodo,
            onToggleTodo,
            onRemoveTodo,
            onChangeFilter,
            onClearCompleted,
        } = useViewProps(props);

        return (
            <div>
                <NewTodoInput onSubmit={onAddTodo} />

                <ul>
                    {filteredTodos.map((todo) => (
                        <li key={todo.id}>
                            <input
                                type="checkbox"
                                checked={todo.done}
                                onChange={() => onToggleTodo(todo.id)}
                            />
                            <span>{todo.text}</span>
                            <button onClick={() => onRemoveTodo(todo.id)}>
                                削除
                            </button>
                        </li>
                    ))}
                </ul>

                <footer>
                    <span>{remainingCount} items left</span>
                    {FILTERS.map(
                        (filterName) => (
                            <button
                                key={filterName}
                                disabled={currentFilter === filterName}
                                onClick={() => onChangeFilter(filterName)}
                            >
                                {filterName}
                            </button>
                        ),
                    )}
                    <button onClick={onClearCompleted}>
                        Clear completed
                    </button>
                </footer>
            </div>
        );
    };

ビューと状態の定義が非常に自明になっているのが見て取れると思う。素朴にコンポーネントを記述する場合、ビューと状態は密になり、それぞれが互いの事情を反映しがちになる。これは小さなコンポーネントであれば素早いコーディングの役に立つが、コンポーネントが大きくなるにつれコンポーネントの成長と可読性を妨げるようになる。View / State 分離パターンでは、View と State はそれぞれ独立して設計できるため、それぞれの責務とプラクティスにフォーカスしてコーディングを行える。これにより、状態設計時にこの状態がビューから見て使いやすいかを考える必要はないし、ビュー設計時に状態遷移をどのように起こすかに捉われずに済む。そしてその分離を保ち続けやすいのが、このパターンの第一の利点だ。

これにより、状態の保存形式は、正規化や最小性を重視して設計できる。たとえば、リストの選択状態を ``Set<string>`` で保持しつつ、ビューには ``isSelected: boolean`` のフラグとして渡すといった変換を Selector 層で行える。保存形式を変えてもビューに影響しないし、ビューの表示を変えても保存形式に影響しない。

また、コンポーネントのコードを読む際に、「今読んでいるのは State の定義なのか、Handler なのか、Selector なのか、View なのか」が構造的に明確になる。各パーツは独立して理解でき、全体のデータフローは一方向に保たれるため、コードの追跡が容易になる。これは認知負荷削減にもつながる。

さらにこれらのパーツが独立していることにより、ビューに触れずにテストも書ける。ビューと状態管理が一体化しているコードでは、ビューを含んでのテストが必要になるが、ビューのテストは書くのも保守するのも基本的に難易度が高くなりがちだ。View / State 分離パターンで書かれていれば、Selector / State / Handler はビューから切り離して定義できるため、ビューをテスト対象に含めなくてもいい。

また、React 標準 API だけで実現できるのでライトに使う事ができる。 ``useCallback``、 ``useMemo`` をつけなければいけないのが少し煩わしいが、これも `React Compiler <https://react.dev/learn/react-compiler>`_ が普及すれば大幅に記述量を削減して自然な記述のみで済むようになるんじゃないかと期待している。また、React Compiler を使わない場合でも、適宜省略可能な場合は省略して良いだろう。

注意点とプラクティス
--------------------------

View / State 分離パターンにより、素朴に React 標準 API だけでコンポーネントの保守性を向上できる。一方で、少しボイラープレート的な記述が増え、また間接参照が増えるため、単純で枯れたコンポーネントには過剰な対応になるかもしれない。このパターンはコンポーネントが成長していくことを前提としており、ボタンやアイコンのような、状態を持たないか極めて単純な状態しか持たないコンポーネントには ``useState`` で十分だろう。目安として、状態遷移が複数あり、ハンドラが3つ以上あるようなコンポーネントでこのパターンの恩恵が大きくなる。

また、このパターンで注意すべき点が、状態のライフサイクルとコンポーネントのライフサイクルが密結合になるという点だ。そのため、コンポーネントとライフサイクルが一致しない状態は別の管理方法をとった方が良い。例えば、アプリケーション全体や複数コンポーネント間での状態共有は、 `Jotai`_ や `Zustand`_ などのより状態管理に特化したライブラリの併用が必要になる。API 通信のキャッシュなどは専用の `Tanstack Query <https://tanstack.com/query/latest>`_ なども検討が必要だろう。ただ、それらのライブラリからコンポーネントローカルに取得した値を ``useViewProps`` 内で扱うことで、両者を組み合わせて使うことは可能だ。

コンポーネントが大きくなったときの分割を考える際も注意が必要だ。この場合、 ``ViewProps`` の肥大化をトリガーにするのではなく、状態のライフサイクルに着目すると良い分割が進みやすい。たとえば、フォームの入力状態とモーダルの開閉状態はライフサイクルが異なるため、それぞれ別の ``useViewProps`` （あるいは別のコンポーネント）に分けることが自然な分割になる。

状態のライフサイクルは多くの場合コンポーネントに密接に関連したものになり、そのライフサイクルに注目してコード設計していくのは多くの場合いい設計をもたらすと経験的に思っている。一方で状態がコンポーネントと独立のライフサイクルを持っていることもあり、そのような場合にはこのデザインパターンの枠組みには当てはまらない。そこのバランスさえ注意すれば、多くのアプリケーションに適用できるパターンなんじゃないかなと思っているので、もし興味持ったらぜひ使ってみてほしい。

まとめ
----------

View / State 分離パターンは、React コンポーネントを State / Handler / Selector / View の4つのパーツで構築し、 ``useViewProps`` カスタムフックによってビューと状態管理を分離するデザインパターンだ。このパターンにより、ビューと状態の定義を明示的に切り離す事ができ、それぞれの関心事にフォーカスしたコーディングを行いやすくなる。一方で、状態とコンポーネントのライフサイクルが一致する場合にのみこのパターンは使う事ができ、それ以外の場合は Zustand や Tanstack Query などとの併用を考える必要があるだろう。

このパターンを使い始めたのは実は結構前で、ずっと記事のネタとして眠っていてずっと記事を書きたいなあとは思っていたが時間が取れなかった。ただ、最近 Claude Code を調教するために記事を書く具体的な需要が出てきて、重い腰上げたって感じ。結構実感としては便利なパターンなんだけど、一方で状態のライフサイクルを見誤った時に結構逆撃を被ったりもして、まだちょっと慣れてないと感じる場面も多いので、もう少しプラクティスを貯めていきたいなあという感じでもある。フロントエンド開発は難しいね。てことで、今回はこれで。
