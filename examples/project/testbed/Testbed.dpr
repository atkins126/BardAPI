(****************************************************************************
                  ::
                 =+=-
                -====:
               -======:
             :=========-.
          .:==========----.
      .:=++==========--------::.
     ==++++=========------------:
      .-=++=========---------::.
          .:========------.
             .=======---.
               -======:          .
                -====:          :=-
                 -==:         .-====.
                  ..       .:=======+=-:
                           .:-==+++++=:.
                               -+++=.
                                :=-

   ___                _    _    ___  ___
  | _ ) __ _  _ _  __| |  /_\  | _ \|_ _|
  | _ \/ _` || '_|/ _` | / _ \ |  _/ | |
  |___/\__,_||_|  \__,_|/_/ \_\|_|  |___|
        Delphi Google® Bard™ API

Copyright © 2023-present tinyBigGAMES™ LLC
All Rights Reserved.

Website: https://tinybiggames.com
Email  : support@tinybiggames.com
*****************************************************************************)

program Testbed;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  UTestbed in '..\..\src\UTestbed.pas',
  BardAPI in '..\..\..\src\BardAPI.pas',
  BardAPI.AccessToken in '..\..\..\src\BardAPI.AccessToken.pas' {BardAcessTokenForm},
  BardAPI.WebView2_2 in '..\..\..\src\BardAPI.WebView2_2.pas';

begin
  try
    RunTests;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
