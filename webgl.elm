module Main exposing (main)

{-
   Try adding the ability to crouch or to land on top of the crate.
-}

import AnimationFrame
import Html exposing (Html, text, div)
import Html.Attributes exposing (width, height, style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Time exposing (Time)
import WebGL exposing (Mesh, Shader, Entity)
import Platform.Sub
import Keyboard
import Keyboard.Key exposing (..)
import Window
import Task

type alias Model =
  { window : Window.Size
  , time : Time
  , person : Person
  }

type Msg =
    Resize Window.Size
  | Tick Float
  | Move Vec3

type alias Person =
    { position : Vec3 }

main : Program Never Model Msg
main =
    Html.program
        { init = ( { window = (Window.Size 0 0), time = 0, person = Person (vec3 0 0 -5)}, Task.perform Resize Window.size )
        , view = view
        , subscriptions = subscriptions
        , update = update
        }

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch
  [ AnimationFrame.diffs Tick
  , Keyboard.presses keyPress
  ]

keyPress : Keyboard.KeyCode -> Msg
keyPress code =
  case (Keyboard.Key.fromCode code) of
    Up -> Move (vec3 0 0 1)
    Down -> Move (vec3 0 0 -1)
    e -> Move (vec3 0 0 0)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick elapsed -> ( { model | time = model.time + elapsed}, Cmd.none )
    Move v -> { model | person = Person (Vec3.add model.person.position v) } ! []
    Resize newSize -> { model | window = newSize } ! []
view : Model -> Html msg
view model =
    WebGL.toHtml
        [ width model.window.width
        , height model.window.height
        , style [ ( "display", "block" ) ]
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            lineModel
            { perspective = perspective model }
        ]

perspective : Model -> Mat4
perspective {window, time, person} =
    Mat4.mul
        (Mat4.makePerspective 45 (toFloat window.width / toFloat window.height) 0.01 100)
        (Mat4.makeLookAt person.position (Vec3.add person.position Vec3.k) Vec3.j)

-- Mesh
type alias Vertex =
    { position : Vec3
    , color : Vec3
    }

lineModel : Mesh Vertex
lineModel =
    WebGL.lineStrip [ Vertex (vec3 0 0 0) (vec3 0 0 0), Vertex (vec3 1 1 0) (vec3 -1 -1 0) ]

-- Shaders
type alias Uniforms =
    { perspective : Mat4
    }

vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        varying vec3 vcolor;
        void main () {
            gl_Position = perspective * vec4(position, 1.0);
            vcolor = color;
        }
    |]

fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }
    |]
