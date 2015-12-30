// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System;
open System.IO;

let get_string fileName=
    File.ReadAllText(fileName)

let make_friendly_name (value:String)=   
    let result=seq {
        for i = 0 to value.Length-1 do
            if i=0 then yield Char.ToUpperInvariant(value.[i])
            elif value.[i-1]='_' then yield Char.ToUpperInvariant(value.[i])
            else yield value.[i]}|>Seq.where(fun r->r<>'_')|>String.Concat
    result


let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(p)
    else
        None

let make_nullable (value:string,modif:string[])=
    let test v=(v="not_null")
    if Seq.exists test modif then
        value
    else
        value+"?"

let get_type (value:string, modifiers:string[])=
    let clean=value.ToLowerInvariant().Trim().Replace(" ","")
    let result=match clean with
                |Prefix "date" v->make_nullable("DateTime",modifiers)
                |Prefix "datetime" v->make_nullable("DateTime",modifiers)
                |Prefix "int" v->make_nullable("int",modifiers)
                |Prefix "char(1)" v->make_nullable("bool",modifiers)
                |Prefix "char" v->"string"
                |Prefix "varchar" v-> "string"
                |Prefix "decimal" v-> make_nullable("decimal",modifiers)
                |Prefix "tinyint" v-> make_nullable("int",modifiers)
                |Prefix "time" v->make_nullable("DateTime",modifiers)
                |Prefix "bit" v->make_nullable("bool",modifiers)                
                |_->"None"
    result

let string_to_tuple (value:String)=
    let components=value.Split([|' '|])
    (make_friendly_name components.[0], get_type (components.[1],components.[2..]))

let analyze (s:string)=
    s.Split([|"\r\n"|],StringSplitOptions.RemoveEmptyEntries)
    |>Seq.map(fun s->s.Trim())
    |>Seq.map(fun s-> string_to_tuple s)
    |>Seq.map(fun (n,t)->String.Format("public {0} {1} {{ get; set;}}", t,n))
    |>String.concat (Environment.NewLine+Environment.NewLine);

[<EntryPoint>]
let main argv = 
    let files=[|("Vehicle.txt","VehicleOut.txt");("VehicleSpec.txt","VehicleSpecOut.txt")|]
    let result=files|>Seq.map (fun (t,d)->t)|>Seq.map get_string |> Seq.map analyze
    Seq.zip files result|>Seq.map (fun((s,t),r)->(t,r))|>Seq.iter(fun (f,r)->File.WriteAllText(f,r))
    0 // return an integer exit code