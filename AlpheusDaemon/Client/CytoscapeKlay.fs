module CytoscapeKlay

open Fable.Core
open Cytoscape
open Cytoscape.Cytoscape

let [<Import("default","cytoscape-klay")>] klay: Cytoscape.Ext = jsNative