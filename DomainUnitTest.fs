namespace AmigoSecreto

open NUnit.Framework
open Sorteing

module Test=

    [<Test>] // Revisar no funciona
    let MakeNotificationsGoneRight () =
        let part1 = { Nombre="Maria"; Email= EmailAddress "maria@maria.com"; Status=Guest}
        let part2 = { Nombre="Jean"; Email= EmailAddress "jean@jean.com"; Status=Guest}
        let part3 = { Nombre="Rebeca"; Email= EmailAddress "rebeca@rebeca.com"; Status=Guest}
        let part4 = { Nombre="Latisha"; Email= EmailAddress "latisha@lathisa.com"; Status=Host}
        
        let sorteishon = {
          Participantes = [part1; part2; part3; part4];
          MontoRegalo = Some(10000);
          Lugar = Some("Casa Jenny");
          Pares = None;
        }
        let sort2 = makePairs sorteishon
        match sort2.Pares with
        | Some x -> Assert.IsTrue((printfn "Enviando Notificacion a usuario Jean con EmailAddress \"jean@jean.com\"").Equals(sendNotification (List.item 0  x)))
        | _ -> None |> ignore  

    [<Test>]
    let MakeRightDrawWith3Entries () =
        let part1 = { Nombre="Maria"; Email= EmailAddress "maria@maria.com"; Status=Guest}
        let part2 = { Nombre="Jean"; Email= EmailAddress "jean@jean.com"; Status=Guest}
        let part3 = { Nombre="Rebeca"; Email= EmailAddress "rebeca@rebeca.com"; Status=Guest}
        let part4 = { Nombre="Latisha"; Email= EmailAddress "latisha@lathisa.com"; Status=Host}
        
        let sorteishon = {
          Participantes = [part1; part2; part3; part4];
          MontoRegalo = Some(10000);
          Lugar = Some("Casa Jenny");
          Pares = None;
        }

        let paresVerif = Some([(part1, part2); (part2,part3); (part3,part4); (part4, part1)]);
        let sorteishonModif = makePairs sorteishon
        Assert.IsTrue(paresVerif.Equals(sorteishonModif.Pares))

    [<Test>]
    let TestModifSeq4elem () =
      
        let part1 = { Nombre="Maria"; Email= EmailAddress "maria@maria.com"; Status=Guest}
        let part2 = { Nombre="Jean"; Email= EmailAddress "jean@jean.com"; Status=Guest}
        let part3 = { Nombre="Rebeca"; Email= EmailAddress "rebeca@rebeca.com"; Status=Guest}
        let part4 = { Nombre="Latisha"; Email= EmailAddress "latisha@lathisa.com"; Status=Host}
        
        let sorteishon = {
          Participantes = [part1; part2; part3; part4] ;
          MontoRegalo = Some(10000);
          Lugar = Some("Casa Jenny");
          Pares = None;
        }

        let q = modifSeq 0 sorteishon
        let fgh =  [part2; part3; part4;part1]
        Assert.IsTrue(q.Equals(fgh))
   
    [<Test>]
    let TestModifSeq3elem () =
      
        let part1 = { Nombre="Maria"; Email= EmailAddress "maria@maria.com"; Status=Guest}
        let part2 = { Nombre="Jean"; Email= EmailAddress "jean@jean.com"; Status=Guest}
        let part3 = { Nombre="Rebeca"; Email= EmailAddress "rebeca@rebeca.com"; Status=Host}

        let sorteishon = {
          Participantes = [part1; part2; part3] ;
          MontoRegalo = Some(10000);
          Lugar = Some("Casa Jenny");
          Pares = None;
        }

        let q = modifSeq 0 sorteishon
        let fgh =  [part2; part3; part1]
        Assert.IsTrue(q.Equals(fgh))
     
    [<Test>]
    let TestRejectedWrongConstructedEmail () =
        let res =  CreateEmailAddress "aasd@asda"
        Assert.IsTrue (Error(InvalidParticipante "Email Invalido") = res)
        
    [<Test>]  
    let TestAceptedRigthConstructedEmail () =   
        let res2 =  CreateEmailAddress "volivaresh@gmail.com"
        Assert.IsTrue (Ok(EmailAddress "volivaresh@gmail.com") = res2)
