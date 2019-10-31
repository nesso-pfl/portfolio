module Page.Biography.Job where

import Prelude
import Plugin.HalogenR (divC, divCE, divC1, h1C1, h2C1, spanC1, buttonCE1)

import Data.Const (Const)
import Effect.Aff (Aff)
import Effect.Class.Console (log)

import Halogen as H
import Halogen.HTML (HTML, text)
import Halogen.HTML.Elements (br_)
import Html.Renderer.Halogen as RH


ui :: H.Component HTML (Const Unit) Unit Void Aff
ui = H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval H.defaultEval
    }


type Slot = H.Slot (Const Unit) Void

render :: ∀ i. Unit -> H.ComponentHTML i () Aff
render _ =
    divC "page biography-job"
        [ divC1 "title-area" ( h1C1 "title" $ text "職務経歴" )
        , divC "main-area"
            [ divC "left-area"
                [ divC "basic-info"
                    [ divC1 "title-content" ( h2C1 "title" $ text "基本情報" )
                    , divC "grid"
                        [ divC1 "dt furigana" $ text "ﾌﾘｶﾞﾅ:"
                        , divC1 "dd furigana" $ text "ﾓﾘｵｶ ｼｭｳｺﾞ"
                        , divC1 "dt name" $ text "氏名:"
                        , divC1 "dd name" $ text "盛岡 修悟"
                        , divC1 "dt" $ text "性別:"
                        , divC1 "dd" $ text "男"
                        , divC1 "dt" $ text "生年月日:"
                        , divC1 "dd" $ text "1992年2月24日"
                        , divC1 "dt" $ text "最寄り駅:"
                        , divC1 "dd" $ text "京王線 千歳烏山駅"
                        , divC1 "dt" $ text "最終学歴:"
                        , divC1 "dd" $ text "私立 高等学校 卒業"
                        ]
                    ]
                , divC "skill-info"
                    [ divC "title-content" 
                        [ h2C1 "title" $ text "技術情報"
                        , spanC1 "remarks" $ text "※ 半年以上ブランクがあるものは * を付けています"
                        ]
                    , divC "grid"
                        [ divC1 "dt" $ text "OS:"
                        , divC1 "dd" $ text "Windows, Linux, macOS"
                        , divC1 "dt" $ text "言語:"
                        , divC "dd"
                            [ text "HTML(Pug), CSS(Sass)"
                            , br_
                            , text "JavaScript(TypeScript*, Elm, PureScript)"
                            , br_
                            , text "Python, Java*, PHP*, Haskell*, C*" ]
                        , divC1 "dt" $ text "FW等:"
                        , divC "dd"
                            [ divC "fw"
                                [ spanC1 "" $ text "[JavaScript]"
                                , spanC1 "" $ text "Yarn, Babel, Parcel, Vue.js, Nuxt.js, JQuery*"
                                ]
                            , divC "fw"
                                [ spanC1 "" $ text "[Python]"
                                , spanC1 "" $ text "Django, scikit-learn"
                                ]
                            , divC "fw"
                                [ spanC1 "" $ text "[PHP]"
                                , spanC1 "" $ text "Laravel*, CakePHP, Composer"
                                ]
                            , divC "fw"
                                [ spanC1 "" $ text "[Java]"
                                , spanC1 "" $ text "Spring Boot*, Struts2*"
                                ]
                            ]
                        , divC1 "dt" $ text "サーバー:"
                        , divC1 "dd" $ text "Apache, IIS, http-server等"
                        , divC1 "dt" $ text "DB:"
                        , divC1 "dd" $ text "MySQL*, SQLite, AWS DynamoDB"
                        , divC1 "dt" $ text "その他:"
                        , divC1 "dd" $ text "Docker, AWS, Firebase, IBM Watson*"
                        ]
                    ]
                ]
            , divC1 "right-area"
                ( divC "exp-info"
                    [ divC1 "title-content" ( h2C1 "title" $ text "業務経験" )
                    , divC "exp"
                        [ divC1 "title" $ text "書類作成支援サービス"
                        , divC1 "date" $ text "2018/02 ~ 2018/12"
                        , divC "grid"
                            [ divC1 "dt" $ text "使用言語:"
                            , divC1 "dd" $ text "Python, HTML, CSS, JavaScript, Java"
                            , divC1 "dt" $ text "OS/DB/その他:"
                            , divC1 "dd" $ text "Windows/MySQL/Django, JQuery"
                            , divC1 "dt" $ text "コメント:"
                            , divC1 "dd" $ text "実務でのシステム開発、仕様書等書類の作成等を経験した。"
                            ]
                        ]
                    , divC "exp"
                        [ divC1 "title" $ text "店内の商品検索サービス"
                        , divC1 "date" $ text "2018/06 ~ 2018/09"
                        , divC "grid"
                            [ divC1 "dt" $ text "使用言語:"
                            , divC1 "dd" $ text "PHP, HTML, CSS, JavaScript, Python"
                            , divC1 "dt" $ text "OS/DB/その他:"
                            , divC1 "dd" $ text "Windows/SQLite/IBM Watson"
                            , divC1 "dt" $ text "コメント:"
                            , divC "dd"
                                [ text "既存レガシーシステムを引き継いでのプロジェクトだった。"
                                , br_
                                , text $ "小規模であり、ディレクターが客先での打ち合わせを"
                                , br_
                                , text $ "サポートしてくれる以外は自分で実装から全て行った。"
                                , br_
                                , text $ "一部情報収集にPythonでWebページのスクレイピングを行った。"
                                , br_
                                , text $ "IBM Watsonを利用した曖昧なキーワード検索を実装した。"
                                ]
                            ]
                        ]
                    , divC "exp"
                        [ divC1 "title" $ text "店頭セルフ受付システム"
                        , divC1 "date" $ text "2018/12 ~ 2019/08"
                        , divC "grid"
                            [ divC1 "dt" $ text "使用言語:"
                            , divC1 "dd" $ text "HTML, CSS, JavaScript"
                            , divC1 "dt" $ text "OS/DB/その他:"
                            , divC1 "dd" $ text "Windows/Nuxt.js"
                            , divC1 "dt" $ text "コメント:"
                            , divC "dd"
                                [ text "2社協業のプロジェクトで、フロントエンドを担当した。"
                                , br_
                                , text $ "そこそこに大規模であり、複雑なGit管理などを経験した。"
                                , br_
                                , text $ "設計や要件に関する打ち合わせにも積極的に関わった。"
                                ]
                            ]
                        ]
                    , divC "exp"
                        [ divC1 "title" $ text "商品情報検索サービス"
                        , divC1 "date" $ text "2019/06 ~ 現在"
                        , divC "grid"
                            [ divC1 "dt" $ text "使用言語:"
                            , divC1 "dd" $ text "Pug, Sass, JavaScript"
                            , divC1 "dt" $ text "OS/DB/その他:"
                            , divC1 "dd" $ text "Windows/Dynamo DB/AWS, Vue.js"
                            , divC1 "dt" $ text "コメント:"
                            , divC "dd"
                                [ text "テクニカルリーダーを務め、技術選定等も行った。"
                                , br_
                                , text $ "要件も客先と話し合い、提案する立場に立った。"
                                , br_
                                , text $ "コードレビュー等を行い、品質の向上に貢献した。"
                                , br_
                                , text $ "サーバーレスなAWSのインフラ構築を経験した。"
                                ]
                            ]
                        ]
                    ]
                )
            ]
        ]
