open System;
open System.IO;
open System.Text.RegularExpressions;

let get_string fileName=
    File.ReadAllText(fileName)

let make_friendly_name (value:String)=   
    let result=seq {
        for i = 0 to value.Length-1 do
            if i=0 then yield Char.ToUpperInvariant(value.[i])
            elif value.[i-1]='_' then yield Char.ToUpperInvariant(value.[i])
            else yield value.[i]}|>Seq.where(fun r->r<>'_')|>String.Concat
    result

let make_nullable (value:string,modif:string[])=
    let test v=(v="not_null")
    if Seq.exists test modif then
        value
    else
        value+"?"

let get_type_info (value:string)=
    let components=Regex.Match(value,"^(?<type>[a-z_]*)(\((?<value>[0-9,]*)\))*$")
    (components.Groups.["type"].Value, components.Groups.["value"].Value)

let get_type (value:string, modifiers:string[])=
    let clean=value.ToLowerInvariant().Trim().Replace(" ","")
    let type_info=get_type_info clean
    let result=match type_info with
                |("date",_) ->(make_nullable("DateTime",modifiers),"","")
                |("datetime",_)->(make_nullable("DateTime",modifiers),"","")
                |("int",_)->(make_nullable("int",modifiers),"","")
                |("char","1")->(make_nullable("bool",modifiers),"","")
                |("char",v)->("string","nvarchar",v)
                |("varchar",v)->("string","nvarchar",v)
                |("decimal",_)->(make_nullable("decimal",modifiers),"","")
                |("tinyint","1")->(make_nullable("bool",modifiers),"","")
                |("tinyint",_)->(make_nullable("int",modifiers),"","")
                |("time",_)->(make_nullable("DateTime",modifiers),"","")
                |("bit",_)->(make_nullable("bool",modifiers),"","")
                |("text",_)->("string","text","")
                |("smallint",_)->(make_nullable("int",modifiers),"","")
                |_->("None","","")
    result

let string_to_tuple (value:String)=
    let components=value.Split([|' '|])
    (make_friendly_name components.[0], get_type (components.[1],components.[2..]))

let gen_property (name:String, ptype:String*String*String)=  
    match ptype with
        |(ct,dt,s)->
            let columnAttr=if String.IsNullOrEmpty(dt) then "" else String.Format("[Column(TypeName=\"{0}\")]{1}", dt,Environment.NewLine)
            let sizeAttr=if String.IsNullOrEmpty(s) then "" else String.Format("[MaxLength({0})]{1}",s, Environment.NewLine)
            String.Format("{2}{3}public {0} {1} {{ get; set;}}", ct,name,columnAttr,sizeAttr)
        
let analyze (s:string)=
    s.Split([|"\r\n"|],StringSplitOptions.RemoveEmptyEntries)
    |>Seq.map(fun s->s.Trim())
    |>Seq.map(fun s-> string_to_tuple s)
    |>Seq.map gen_property
    |>String.concat (Environment.NewLine+Environment.NewLine);

let write_to_file (file:String) (value:String)=
    File.WriteAllText(file,value)  

[<EntryPoint>]
let main argv =         
    let files=[|"Vehicle.txt";"VehicleSpec.txt"|]
    let resultFile="VehicleOut.txt"
    let res=files|>Seq.map get_string |> Seq.map analyze|>String.concat Environment.NewLine|>write_to_file resultFile    
    0
