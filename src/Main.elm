module Main exposing (Memory, init, main, update, view)

import Array exposing (Array)
import Playground exposing (..)
import Playground.Extra exposing (tile)



-- MAIN


main : Program () (Playground Memory) Msg
main =
    game view update init


init : Memory
init =
    { x = 0
    , y = 0
    , vx = 0
    , vy = 0
    , dir = 1
    }


type alias Memory =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , dir : Float
    }



-- VIEW


view : Computer -> Memory -> List Shape
view computer ori =
    let
        w =
            computer.screen.width

        h =
            computer.screen.height

        b =
            computer.screen.bottom
    in
    [ rectangle (rgb 174 238 238) w h
    , rectangle (rgb 74 163 41) w 100
        |> moveY b
    , tile 128 128 spriteSheet (getFrame ori computer.time)
        |> scale 1
        |> scaleX ori.dir
        |> move ori.x (b + 76 + ori.y)
    ]


getFrame : Memory -> Time -> number
getFrame ori time =
    let
        frame =
            .now time // 60 |> remainderBy 8
    in
    if ori.y > 0 then
        5

    else if ori.vx /= 0 then
        Array.get frame run |> Maybe.withDefault 0

    else
        0



-- UPDATE


update : Computer -> Memory -> Memory
update computer ori =
    let
        dt =
            toFloat (.delta computer.time) / 10

        vx =
            toX computer.keyboard

        vy =
            if ori.y == 0 then
                if computer.keyboard.up then
                    9

                else
                    0

            else
                ori.vy - dt / 4

        x =
            ori.x + dt * vx * 2

        y =
            ori.y + dt * vy
    in
    { x = x
    , y = max 0 y
    , vx = vx
    , vy = vy
    , dir =
        if vx == 0 then
            ori.dir

        else if vx < 0 then
            1

        else
            -1
    }


run : Array number
run =
    Array.fromList [ 1, 2, 3, 4, 5, 6, 7, 8 ]


spriteSheet : String
spriteSheet =
    "../sprites/spriteSheet1.png"
