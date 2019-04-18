module CytoscapeDagre

open Fable.Core
open Fable.Helpers.React
open Fable.Import.React
open Fable.Core.JsInterop
open Cytoscape
open Fable.Helpers.React.Props

let [<Import("default","cytoscape-dagre")>] dagre: Cytoscape.Ext = jsNative

