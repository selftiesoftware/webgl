module Main exposing (main)

{-
   Try adding the ability to crouch or to land on top of the crate.
-}

import AnimationFrame
import Html exposing (Html, text, div)
import Html.Attributes exposing (width, height, style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Time exposing (Time)
import WebGL exposing (Mesh, Shader, Entity)
import Platform.Sub
import Keyboard
import Keyboard.Key exposing (..)
import Window
import Mouse
import Task

type alias Model =
  { window : Window.Size
  , mouse : Maybe Vec2
  , time : Time
  , person : Person
  }

type Msg =
    Resize Window.Size
  | Tick Float
  | MoveMouse Vec2
  | MoveKey Vec2

type alias Person =
    { position : Vec3 }

main : Program Never Model Msg
main =
    Html.program
        { init = ( { window = (Window.Size 0 0), mouse = Nothing, time = 0, person = Person (vec3 0 0 -5)}, Task.perform Resize Window.size )
        , view = view
        , subscriptions = subscriptions
        , update = update
        }

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch
  [ AnimationFrame.diffs Tick
  , Keyboard.presses keyPress
  , Mouse.moves mouseMove
  ]

keyPress : Keyboard.KeyCode -> Msg
keyPress code =
  case (Keyboard.Key.fromCode code) of
    Up -> MoveKey (vec2 0 1)
    Down -> MoveKey (vec2 0 -1)
    Left -> MoveKey (vec2 1 0)
    Right -> MoveKey (vec2 -1 0)
    e -> MoveKey (vec2 0 0)

mouseMove : Mouse.Position -> Msg
mouseMove {x, y} = MoveMouse (vec2 (toFloat x) (toFloat y))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick elapsed -> ( { model | time = model.time + elapsed}, Cmd.none )
    MoveKey v -> { model | person = Person (Vec3.add model.person.position (vec3 (Vec2.getX v) 0 (Vec2.getY v))) } ! []
    MoveMouse v -> case model.mouse of
      Nothing -> { model | mouse = Just v } ! []
      Just mouse ->
        let
          scale = (Vec3.getZ model.person.position) / 1000
          mouseDelta = Vec2.scale scale (Vec2.sub mouse v)
          mouse3D = vec3 (Vec2.getX mouseDelta) (Vec2.getY mouseDelta) 0
        in
        { model | mouse = Just v, person = Person (Vec3.add model.person.position mouse3D) } ! []
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
    let
      l x y = ( Vertex (vec3 (toFloat x / 10) 0 (toFloat y / 10)) (vec3 0 0 0), Vertex (vec3 1 (toFloat x / 10) 0) (vec3 -1 -1 0) )
      xs = List.range 0 1000
      ys = List.range 0 1000
    in
      WebGL.lines <| List.map2 l xs ys

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
