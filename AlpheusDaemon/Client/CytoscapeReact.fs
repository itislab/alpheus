module CytoscapeReact

open Fable.Core
open Fable.React
open Fable.React.Props
open Shared
open Cytoscape
//open Browser

//type CytoscapeState = { counter: int }
type CytoscapeReactProps = { graph: AlpheusGraph }

type CytoscapeReact(initialProps) =
    inherit PureStatelessComponent<CytoscapeReactProps>(initialProps)

    //let mutable ref : Element = null

    //override this.componentDidMount() =
    //    let s = 
    //    let container = ref
    //    let opts = createEmpty<Cytoscape.CytoscapeOptions>
    //    opts.container <- Some (upcast container.)
    //    ()

    override this.componentDidUpdate (prevProps, _) = ()

    override this.render() =
        div [ ] [ str "Counter = "; ofInt 5 ]

let inline counterDisplay p = ofType<CytoscapeReact,_,_> p []

//type CytoscapeReact(initialProps) as this =
//    inherit Compo Component<obj, CounterState>(initialProps)
//    do
//        this.setInitState({ counter = 0})

//    // This is the equivalent of doing `this.add = this.add.bind(this)`
//    // in javascript (Except for the fact that we can't reuse the name)
//    let add = this.Add
//    let remove = this.Remove

//    member this.Add(_:MouseEvent) =
//        this.setState({ counter = this.state.counter + 1 })

//    member this.Remove(_:MouseEvent) =
//        this.setState({ counter = this.state.counter - 1 })

//    override this.render() =
//        div [] [
//            counterDisplay { CounterDisplayProps.counter = this.state.counter }
//            addRemove { add = add; remove = remove }
//        ]

//let inline counter props = ofType<Counter,_,_> props []