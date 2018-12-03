module Day2

open System.IO
open System

let data = File.ReadAllLines("day2input.txt") |> Array.map Int32.Parse
