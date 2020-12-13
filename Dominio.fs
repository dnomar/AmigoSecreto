namespace AmigoSecreto

open System

type EmailAddress = EmailAddress of string
type Status = Host|Guest

type ErrorMessage = 
    | InvalidSorteo of string
    | InvalidParticipante of string

type Commands = 
    | CreateNewSorteo of string

type EventTypes = 
    | SorteoRealizado

type Events = 
    {
        Type: EventTypes;
        Msg: string;
    }

type Participante = {
    Nombre:string;
    Email:EmailAddress;
    Status:Status;
}

type Sorteo ={
    Participantes:(Participante)list;
    MontoRegalo:int option;
    Lugar:string option;
    Pares: (Participante * Participante)list option;
}

module Sorteing =

    let testApi = 
        "Hello for Sorteishon"
    
    let CreateEmailAddress (s:string) = 
        if System.Text.RegularExpressions.Regex.IsMatch(s,@"^\S+@\S+\.\S+$") 
            then Ok (EmailAddress s)
            else Error (InvalidParticipante "Email Invalido")

    let validParticipantes secretFriend =
        match Seq.length secretFriend.Participantes with
        | (x:int) when x < 2 -> Error (InvalidSorteo "Faltan Participantes para realizar el sorteo")
        | _ -> Ok (secretFriend)

    // Modifica el orden de la secuencia corriendo 1 espacio la secuencia y el 
    // primer elemento pasa a ser el último
    let rec modifSeq step secretFriend = 
        seq{
            let part = secretFriend.Participantes
            if step <= ((List.length part)-1) then
                match step = ((List.length part) - 1 ) with
                | true -> yield List.item  0 part
                | false -> yield List.item (step + 1) part
                yield! modifSeq (step+1) secretFriend            
        } |> Seq.toList   

    //Genera las parejaspara realizar el amigo secreto
    let makePairs secretFriend =
        let part = secretFriend.Participantes
        let partModif = modifSeq 0 secretFriend
        { secretFriend with Pares = Some (List.zip part partModif) }
       
    let sendNotification (a:Participante, b:Participante) = 
        printfn "Enviando Notificacion a usuario %s con %A" b.Nombre b.Email    
     
    let notifyEntries secretFriend = 
        match secretFriend.Pares with
        | Some x -> List.iter sendNotification x
        | None -> None |> ignore

    let makeDraw secretFriend = 
        secretFriend
        |> validParticipantes 

    let EventHandler event =
        match event.Type with
        | SorteoRealizado -> printfn "Se realizó todo lo que es el sorteo %A" event.Msg 

