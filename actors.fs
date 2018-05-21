type DU =
| A
| B of string

let actor () = MailboxProcessor.Start(fun inbox ->
    let rec loop x = async {
        let! msg = inbox.Receive ()
        printfn "%A %d" x System.Threading.Thread.CurrentThread.ManagedThreadId
        return! loop (msg::x) }
    
    loop [])


let a = actor ()
let b = actor ()

a.Post "X"
a.Post "X"
b.Post "X"
b.Post "X"