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

unit UTestbed;

interface

uses
  System.SysUtils,
  BardAPI;

procedure RunTests;

implementation

procedure Pause;
begin
  WriteLn;
  Write('Press ENTER to continue...');
  ReadLn;
  WriteLN;
end;

{ ---------------------------------------------------------------------------
  This example presents a fundamental demonstration of engaging in a
  contextual conversation with Bard.
----------------------------------------------------------------------------- }
procedure Test01;
var
  LBardAPI: TBardAPI;
  LPrompt: string;
  LImage: TBardAPIImageItem;
  LCode: TBardAPICodeItem;
begin
  WriteLn('Hi, I am Bard. How may I help you?'#10#13);

  // init Bard instance
  LBardAPI := TBardAPI.Create;
  try
    // init proxy settings if needed
    //LBardAPI.SetProxy(...)

    // init access token, setting to blank try to get from cached value else
    // it will open a window for you to logo into Bard and create one, then
    // cache and return this value. Just log in with a valid google account
    // then close the window and it will fetch the token.
    LBardAPI.AccessToken := '';

    // start the conversation loop
    repeat

      // wait for user to input a question
      Write('Q: ');
      ReadLn(LPrompt);

      // check for quit, then exit loop
      if SameText(LPrompt, '/q') or SameText(LPrompt, '/quit') then
        Break;

      // set input prompt to the question for Bard to query
      LBardAPI.Question := LPrompt;

      // query Bard with question
      LBardAPI.Query;

      // check result status
      if LBardAPI.Success then
        begin
          // success - response to query will be in TBardAPI.Answer
          WriteLn(#13#10'A: ', LBardAPI.Answer);

          // check for images
          if Assigned(LBardAPI.ImageList) then
          begin
            WriteLn;
            for LImage in LBardAPI.ImageList do
            begin
              WriteLn(Format('%s (%s)', [LImage.ImageUrl, LImage.RefUrl]));
            end;
          end;

          // check for code
          if Assigned(LBardAPI.CodeList) then
          begin
            WriteLn('===[ Code Blocks ] ==============================');
            for LCode in LBardAPI.CodeList do
            begin
              WriteLn('Language: ', LCode.Language);
              WriteLn(LCode.Code);
            end;
            WriteLn('-------------------------------------------------');
          end;

          WriteLn;
        end
      else
        begin
          // failed - error message will be in TBardAPI.Answer
          WriteLn(#13#10, LBardAPI.Answer, #13#10);
         end;

    until False;

  finally
    // free Bard instance
    LBardAPI.Free;
  end;
end;

{ ---------------------------------------------------------------------------
----------------------------------------------------------------------------- }
procedure TestXX;
begin
end;

procedure RunTests;
begin
  Test01;
  Pause;
end;

end.
