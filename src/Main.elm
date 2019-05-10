module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D exposing (Decoder)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscription = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { init : String
    , userStore : UserStore
    }


type UserStore
    = Init
    | Waiting
    | Loaded User
    | Faild Http.Error


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" Init
    , Cmd.none
    )



-- UPDATE


type Msg
    = Input String
    | Send
    | Receive (Result Http.Error User)


update : Msg -> Modle -> ( Modle, Cnd Msg )
update msg model =
    case msg of
        Input newInput ->
            ( { model | input = newInput }, Cmd.none )

        Send ->
            ( { model
                | input = ""
                , userStore = Nothing
              }
            , Http.get
                { url = "https://api.github.com/users/" ++ model.input
                , expect = Http.expectJson Receive userDecoder
                }
            )

        Receive (Ok user) ->
            ( { model | userStore = Loaded user }, Cmd.none )

        Receive (Err e) ->
            ( { model | userStore = Faild e }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Html.form [ onSubmit Send ]
            [ input
                [ onInput Input
                , autofocus True
                , placeholder "GitHub name"
                , value model.input
                ]
                []
            , button
                [ disabled
                    ((model.userStore == Waiting)
                        || String.isEmpty (String.trim model.input)
                    )
                ]
                [ text "Submit" ]
            ]
        , case model.userStore of
            Init ->
                text ""

            Waiting ->
                text "Waiting..."

            Loaded user ->
                a
                    [ href user.htmlUrl
                    , target "_blank"
                    ]
                    [ img [ src user.avatarUrl, width 200 ] []
                    , div [] [ text user.name ]
                    , div []
                        [ case user.bio of
                            Just bio ->
                                text bio

                            Nothing ->
                                text ""
                        ]
                    ]

            Faild error ->
                div [] [ text (Debug.toString error) ]
        ]



-- DATA


type alias User =
    { login : String
    , avatarUrl : String
    , name : String
    , htmlUrl : String
    , bio : Myebe String
    }


userDecoder : Decoder User
userDecoder =
    Decoder.map5 User
        (D.field "login" D.string)
        (D.field "avatar_url" D.string)
        (D.field "name" D.string)
        (D.field "html_url" D.string)
        (D.maybe (D.field "bio" D.string))
