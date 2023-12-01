#r "nuget: Raylib-cs"

open Raylib_cs
open type Raylib

type CBool with member this.AsBool = this = CBool(true)

InitWindow(400, 300, "Bomberman")
SetTargetFPS(60)
DisableEventWaiting()
SetExitKey(LanguagePrimitives.EnumOfValue 0)

while not (WindowShouldClose().AsBool) do
    PollInputEvents()
    BeginDrawing()
    ClearBackground(Color.RAYWHITE)
    EndDrawing()

CloseWindow()
