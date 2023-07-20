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

Inspired by:
  php-bard-api - https://github.com/pj8912/php-bard-api
  Bard-API     - https://github.com/dsdanielpark/Bard-API
*****************************************************************************)

unit BardAPI;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  System.IOUtils,
  System.StrUtils,
  System.Classes,
  System.Net.HttpClient,
  System.Net.URLClient,
  System.NetEncoding,
  System.Math,
  System.JSON,
  System.RegularExpressions,
  BardAPI.AccessToken;

const
  CGoogleBardUrl = 'https://bard.google.com/';

  CTimeOutBase = 60000; // ms
  CTimeOutSecs =  CTimeOutBase * 3; // secs

type
  { TBardAPIImageItem }
  TBardAPIImageItem = record
    ImageUrl: string;
    RefUrl: string;
  end;

  { TBardAPIImageList }
  TBardAPIImageList = array of TBardAPIImageItem;

  { TBardAPICodeItem }
  TBardAPICodeItem = record
    Language: string;
    Code: string;
  end;

  { TBardAPICodeList }
  TBardAPICodeList = array of TBardAPICodeItem;

  { TBardAPIConversationProc }
  TBardAPIConversationProc = procedure(const ASender: Pointer;
    const AConversationTitle, AConversationID, AResponseID, AChoiceID: string);

  { TBardAPIConversationEventHandler }
  TBardAPIConversationEventHandler = record
    Sender: Pointer;
    Handler: TBardAPIConversationProc;
  end;

  { TBardAPI }
  TBardAPI = class
  private
    FAccessToken: string;
    FProxy: TProxySettings;
    FConversationTitle: string;
    FConversationID: string;
    FLastConversationID: string;
    FResponseID: string;
    FChoiceID: string;
    FReqID: Integer;
    FSNlM0e: string;
    FSuccess: Boolean;
    FQuestion: string;
    FAnswer: string;
    FSetConversationFlag: Boolean;
    FOnNewConversation: TBardAPIConversationEventHandler;
    FOnSetConversation: TBardAPIConversationEventHandler;
    FImageList: TBardAPIImageList;
    FCodeList: TBardAPICodeList;
    function  FindJsonValue(const AValue: TJSONValue;
      const APath: string): TJSONValue;
    function  GenAccessToken: string;
    function  GetSNlM0e: string;
    procedure SetAccessToken(const AValue: string);
    procedure DoNewConversation;
    procedure DoSetConversation;
    procedure SetCustomHeaders(const ASession: THttpClient);
    function  SanitizeToJson(const aText: string): string;
    function  SanitizeFromJson(const aText: string): string;
    function  ExplodeString(const ADelimiter, AStr: string): TArray<string>;
    function  BuildHttpQuery(const AParams: TStrings): string;
    function  ExtractImageUrl(const AUrl: string): string;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property AccessToken: string read FAccessToken write SetAccessToken;
    property Success: Boolean read FSuccess;
    property Question: string read FQuestion write FQuestion;
    property Answer: string read FAnswer;
    property ImageList: TBardAPIImageList read FImageList;
    property CodeList: TBardAPICodeList read FCodeList;

    procedure SetProxy(const aHost: string; aPort: Integer;
      const aUserName: string = ''; const aPassword: string = '';
      const AScheme: string = '');
    procedure Query;
    function  SaveConversation(const AFilename: string): Boolean;
    function  LoadConversation(const AFilename: string): Boolean;
    procedure ClearConversation;
    procedure SetConversation(const AConversationID, AResponseID, AChoiceID,
      AConversationTitle: string);

    class function GetDataPath: string;
  end;

implementation

{ TBardAPI }
procedure TBardAPI.SetAccessToken(const AValue: string);
begin
  if AValue.IsEmpty then
    FAccessToken := GenAccessToken
  else
    FAccessToken := AValue;
  FSNlM0e := GetSNlM0e;
end;

procedure TBardAPI.DoNewConversation;
begin
  if Assigned(FOnNewConversation.Handler) then
  begin
    FOnNewConversation.Handler(FOnNewConversation.Sender, FConversationTitle,
      FConversationID, FResponseID, FChoiceID);
  end;
end;

procedure TBardAPI.DoSetConversation;
begin
  if Assigned(FOnSetConversation.Handler) then
  begin
    FOnSetConversation.Handler(FOnSetConversation.Sender, FConversationTitle,
      FConversationID, FResponseID, FChoiceID);
  end;
end;

procedure TBardAPI.SetCustomHeaders(const ASession: THttpClient);
begin
  ASession.CustomHeaders['Host'] := 'bard.google.com';
  ASession.CustomHeaders['X-Same-Domain'] := '1';
  ASession.CustomHeaders['User-Agent'] := 'Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.114 Safari/537.36';
  ASession.CustomHeaders['Content-Type'] := 'application/x-www-form-urlencoded;charset=UTF-8';
  ASession.CustomHeaders['Origin'] := 'https://bard.google.com';
  ASession.CustomHeaders['Referer'] := 'https://bard.google.com/';
  ASession.CustomHeaders['Cookie'] := '__Secure-1PSID=' + FAccessToken;
  ASession.ResponseTimeout := CTimeOutSecs;
  ASession.ConnectionTimeout := CTimeOutSecs;
end;

function  TBardAPI.SanitizeToJson(const aText: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(aText) do
  begin
    case aText[i] of
      '\': Result := Result + '\\';
      '"': Result := Result + '\"';
      '/': Result := Result + '\/';
      #8:  Result := Result + '\b';
      #9:  Result := Result + '\t';
      #10: Result := Result + '\n';
      #12: Result := Result + '\f';
      #13: Result := Result + '\r';
      else
        Result := Result + aText[i];
    end;
  end;
  Result := Result;
end;

function  TBardAPI.SanitizeFromJson(const aText: string): string;
var
  LText: string;
begin
  LText := aText;
  LText := LText.Replace('\n', #10);
  LText := LText.Replace('\r', #13);
  LText := LText.Replace('\b', #8);
  LText := LText.Replace('\t', #9);
  LText := LText.Replace('\f', #12);
  LText := LText.Replace('\/', '/');
  LText := LText.Replace('\"', '"');
  LText := LText.Replace('\\', '\');
  Result := LText;
end;

function TBardAPI.ExplodeString(const ADelimiter, AStr: string): TArray<string>;
var
  Parts: TArray<string>;
  I: Integer;
begin
  Parts := SplitString(AStr, ADelimiter);
  SetLength(Result, Length(Parts));
  for I := 0 to High(Parts) do
    Result[I] := Parts[I];
end;

function TBardAPI.BuildHttpQuery(const AParams: TStrings): string;
var
  KeyValuePairs: TStringList;
  I: Integer;
  Pair: string;
begin
  KeyValuePairs := TStringList.Create;
  try
    for I := 0 to AParams.Count - 1 do
    begin
      Pair := Format('%s=%s', [
        TNetEncoding.URL.Encode(AParams.Names[I]),
        TNetEncoding.URL.Encode(AParams.ValueFromIndex[I])
      ]);
      KeyValuePairs.Add(Pair);
    end;

    Result := AnsiReplaceStr(KeyValuePairs.Text, #13#10, '&');

  finally
    KeyValuePairs.Free;
  end;
end;

function TBardAPI.ExtractImageUrl(const AUrl: string): string;
const
  CImageExts: array[0..22] of string = (
    'jpeg', 'jpg', 'png', 'gif', 'webp', 'svg', 'bmp', 'tiff', 'tif',
    'ico', 'heic', 'heif', 'jp2', 'apng', 'cr2', 'nef', 'orf', 'arw',
    'rw2', 'dng', 'pgm', 'ppm', 'hd');
var
  LExt: string;
  LIndex: integer;
begin
  Result := AUrl;

  for LExt in CImageExts do
  begin
    LIndex :=  AUrl.IndexOf(LExt);
    if LIndex <> -1 then
    begin
      Delete(Result, LIndex+Length(LExt)+1, Length(AUrl));
      Break;
    end;
  end;
end;
function TBardAPI.FindJsonValue(const AValue: TJSONValue;
  const APath: string): TJSONValue;
var
  LParser: TJSONPathParser;
  LCurrentValue: TJSONValue;
begin
  Result := nil;
  if APath = '' then Exit;

  LParser := TJSONPathParser.Create(APath);
  LCurrentValue := AValue;

  while not LParser.IsEof do
  begin
    case LParser.NextToken of

      TJSONPathParser.TToken.Name:
      begin
        if LCurrentValue.ClassType = TJSONObject then
          begin
            LCurrentValue :=
              TJSONObject(LCurrentValue).Values[LParser.TokenName];
            if LCurrentValue = nil then Exit;
          end
        else
          Exit;
      end;

      TJSONPathParser.TToken.ArrayIndex:
      begin
        if LCurrentValue.ClassType = TJSONArray then
          if LParser.TokenArrayIndex < TJSONArray(LCurrentValue).Count then
            LCurrentValue :=
            TJSONArray(LCurrentValue).Items[LParser.TokenArrayIndex]
          else
            Exit
        else
          Exit;
      end;

      TJSONPathParser.TToken.Error,
      TJSONPathParser.TToken.Undefined:
      begin
        Exit;
      end;

      TJSONPathParser.TToken.Eof: ;
    end;
  end;

  Result := LCurrentValue;
end;

function TBardAPI.GenAccessToken: string;
var
  LConfig: TStringList;
  LForm: TBardAcessTokenForm;
  LToken: string;
  LPath: string;
begin
  LPath := GetDataPath+'config.txt';
  LConfig := TStringList.Create;
  try
    LToken := '';
    if TFile.Exists(LPath) then
    begin
      LConfig.LoadFromFile(GetDataPath+'config.txt');
      LToken := LConfig.Values['AccessToken'];
    end;
    if LToken.IsEmpty then
    begin
      LForm := TBardAcessTokenForm.Create(nil);
      try
        LForm.ShowModal;
        LToken := LForm.AcessToken;
        LConfig.Values['AccessToken'] := LToken;
        LConfig.SaveToFile(GetDataPath+'config.txt');
      finally
        LForm.Free;
      end;
    end;
  finally
    LConfig.Free;
  end;

  Result := LToken;
end;

function TBardAPI.GetSNlM0e: string;
var
  LSession: THttpClient;
  Response: IHTTPResponse;
  RespContent: string;
  Matches: TMatchCollection;
begin
  LSession := THttpClient.Create;
  try
    LSession.ProxySettings := FProxy;

    SetCustomHeaders(LSession);
  
    LSession.ResponseTimeout := CTimeOutSecs;
    LSession.ConnectionTimeout := CTimeOutSecs;

    Response := LSession.Get(CGoogleBardUrl);
    if Response.StatusCode <> 200 then
      raise Exception.Create('Response Status: ' +
        IntToStr(Response.StatusCode));

    RespContent := Response.ContentAsString;
    Matches := TRegEx.Matches(RespContent, '"SNlM0e":"(.*?)"');
    if Matches.Count > 0 then
      Result := Matches[0].Groups[1].Value
    else
      raise Exception.Create('SNlM0e value not found');
  finally
    LSession.Free;
  end;
end;

constructor TBardAPI.Create;
begin
  inherited;
  FReqID := RandomRange(Integer(Round(Power(10, 3-1))),
    Integer(Round(Power(10, 3) + 1)));
  FConversationID := '';
  FResponseID := '';
  FChoiceID := '';
  FLastConversationID := '';
  FConversationTitle := '';
  FSNlM0e := '';
  FAccessToken := '';
end;

destructor TBardAPI.Destroy;
begin
  inherited;
end;

class function TBardAPI.GetDataPath: string;
var
  LPath: string;
begin
  LPath := TPath.GetCachePath + '\tinyBigGAMES\BardAPI\';
  if not TDirectory.Exists(LPath) then
    TDirectory.CreateDirectory(LPath);
  Result := LPath;
end;

procedure TBardAPI.SetProxy(const aHost: string; aPort: Integer;
  const aUserName: string = ''; const aPassword: string = '';
  const AScheme: string = '');
begin
  FProxy.Create(aHost, aPort, aUserName, aPassword, aScheme);
end;

procedure TBardAPI.Query;
const
  CParams = 'bl=%s&_reqid=%s&rt=%s';
  CUrl = 'https://bard.google.com/_/BardChatUi/data/assistant.lamda.BardFrontendService/StreamGenerate?' ;
var
  LSession: THttpClient;
  LInputTextStruct: TJSONArray;
  LData: TStringList;
  LUrl: string;
  LRespContent: string;
  LRespDict: TJSONArray;
  LParsedAnswer: TJSONArray;
  LBardAnswer: TJSONObject;
  LPostData: TStringStream;
  LParams: string;
  LText: string;
  LFReq: TJSonArray;
  I: Integer;
  LResponse: IHttpResponse;

  procedure GetImages;
  var
    LJson: TJsonArray;
    LText: string;
    I: Integer;

  begin
    LText := FindJsonValue(LParsedAnswer, '[4][0][4]').ToString;
    if LText = 'null' then Exit;

    LJson := TJSONArray.ParseJSONValue(LText) as TJSONArray;
    try
      SetLength(FImageList, LJson.Count);
      for I := 0 to LJson.Count-1 do
      begin
        // image url
        FImageList[I].ImageUrl := ExtractImageUrl(FindJsonValue(LJson,
          Format('[%d][0][0][0]', [I])).Value);

        // image reference url
        FImageList[I].RefUrl := FindJsonValue(LJson, Format('[%d][1][0][0]',
          [I])).Value;
      end;
    finally
      LJson.Free;
    end;
  end;

  procedure GetCode;
  const
    CodeMarker = '```';
  var
    StartIndex, EndIndex: Integer;
    StartPos, EndPos, CodeBlockCount: Integer;
    LCodeItem: TBardAPICodeItem;
    CodeBlock: string;
  begin
    // Find the first starting code marker
    StartIndex := Pos(CodeMarker, FAnswer);
    if StartIndex = 0 then Exit;

    CodeBlockCount := 0;
    StartPos := StartIndex + Length(CodeMarker);

    repeat
      // Find the ending code marker
      EndIndex := PosEx(CodeMarker, FAnswer, StartPos);
      if EndIndex = 0 then Break;

      // Extract the code block
      CodeBlock := Trim(Copy(FAnswer, StartPos, EndIndex - StartPos));

      // Determine if language is present
      if (StartPos < Length(FAnswer)) and (FAnswer[StartPos] <> #10) then
        begin
          // Extract the language
          EndPos := Pos(#10, CodeBlock);
          if EndPos > 0 then
            begin
              LCodeItem.Language := Trim(Copy(CodeBlock, 1, EndPos - 1));
              CodeBlock := Trim(Copy(CodeBlock, EndPos + 1, Length(CodeBlock) - EndPos));
            end
          else
            begin
              LCodeItem.Language := ''; // No language specified
            end;
        end
      else
        begin
          LCodeItem.Language := ''; // No language specified
        end;

      Inc(CodeBlockCount);
      SetLength(FCodeList, CodeBlockCount);
      LCodeItem.Code := CodeBlock;
      FCodeList[CodeBlockCount - 1] := LCodeItem;

      // Find the next starting code marker
      StartIndex := PosEx(CodeMarker, FAnswer, EndIndex + Length(CodeMarker));
      if StartIndex = 0 then Break;

      StartPos := StartIndex + Length(CodeMarker);
    until False;
  end;

begin
  // init variables
  FSuccess := False;
  FImageList := nil;
  FCodeList := nil;

  // check for valid question
  if FQuestion.IsEmpty then
  begin
    FAnswer := 'No question was set';
    Exit;    
  end;
  
  // check for valid access token
  if FAccessToken.IsEmpty then
  begin
    FAnswer := 'Access token is not set';
    Exit;
  end;

  // check for valid FSNlM0e
  if FSNlM0e.IsEmpty then
  begin
    FAnswer := 'Access token is invalid or expired';
    Exit;    
  end;
  
  // create a http session instance
  LSession := THttpClient.Create;
  try
    // init proxy 
    LSession.ProxySettings := FProxy;

    // init custom headers
    SetCustomHeaders(LSession);
    
    // init https params
    LParams := Format(CParams, ['boq_assistant-bard-web-server_20230419.00_p1',
      IntToStr(FReqID), 'c']).Trim;

    // init input text structure for bard
    LInputTextStruct := TJSONArray.Create;
    try
      // set the questino
      LInputTextStruct.Add(TJSONArray.Create.Add(SanitizeToJson(FQuestion)));

      // init to null
      LInputTextStruct.AddElement(TJSONNull.Create);

      // init IDs for conversation context
      LInputTextStruct.AddElement(TJSONArray.Create.Add(
        FConversationID).Add(FResponseID).Add(FChoiceID));

      // init stringlist for input data
      LData := TStringList.Create;
      try

        // init 'freq' value
        LFReq := TJsonArray.Create(TJSONNull.Create, TJSONString.Create(
          LInputTextStruct.ToString));
        try
          LData.AddPair('f.req', LFReq.ToString);
        finally
          LFreq.Free;
        end;

        // init 'at' value
        LData.AddPair('at', FSNlM0e);

        // build http query
        LText := BuildHttpQuery(LData);
      finally
        // free data instance
        LData.Free;
      end;

      // init post url
      LUrl := CUrl + LParams;

      // init post data
      LPostData := TStringStream.Create(LText, TEncoding.UTF8);
      try
        try
          // http post to Bard
          LResponse := LSession.Post(LUrl, LPostData);

          // get response string
          LRespContent := LResponse.ContentAsString;
        except
          // check any exception
          on E: Exception do
          begin
            // return error
            FAnswer := Format('HTTP status code %d: %s', [LResponse.StatusCode,
              LResponse.StatusText]);
            Exit;
          end;
        end;
      finally
        // free post data instance
        LPostData.Free;
      end;

    finally
      // free input text struct instance
      LInputTextStruct.Free;
    end;

    // get response content from response payload
    LText := ExplodeString(#10, LRespContent)[3];

    // create a json array of the response payload
    LRespDict := TJSONArray.ParseJSONValue(LText) as TJSONArray;
    try
      // check for errors
      if LRespDict.Count = 0 then
      begin
        FAnswer := 'Response Error';
        Exit;
      end;

      // create a json array from the content array
      LParsedAnswer := TJSONArray.ParseJSONValue(LRespDict.Items[0].A[2].Value)
        as TJSONArray;
      try
        // check for errors
        if LParsedAnswer = nil then
        begin
          FAnswer := 'Parsed answer is null';
          Exit;
        end;

        // get images
        GetImages;

        // get response answer
        FAnswer := FindJsonValue(LParsedAnswer, '[4][0][1][0]').Value;

        // get conversation id
        FConversationID := FindJsonValue(LParsedAnswer, '[1][0]').Value;

        // get response id
        FResponseID := FindJsonValue(LParsedAnswer, '[1][1]').Value;

        // get choice id
        FChoiceID := FindJsonValue(LParsedAnswer, '[4][0][0]').Value;

        // update freq id
        FReqID := FReqID + 100000;

        // get code
        GetCode;

        // check if this is a new conversation
        if FLastConversationID <> FConversationID then
        begin
          // update last conversation id
          FLastConversationID := FConversationID;

          // get the conversation title
          if FindJsonValue(LParsedAnswer, '[2]') <> nil then
            if FindJsonValue(LParsedAnswer, '[2][0][0]') <> nil then
              FConversationTitle := FindJsonValue(LParsedAnswer,
                '[2][0][0]').Value
            else
             FConversationTitle := FQuestion;

          // call new conversation event
          DoNewConversation;

          // reset set conversation flag
          FSetConversationFlag := False;
        end;

        // check if a conversation was set
        if FSetConversationFlag then
        begin
          // reset set conversatin flag
          FSetConversationFlag := False;

          // call set conversation event
          DoSetConversation;
        end;

        // set success to true
        FSuccess := True;

      finally
        // free parased answer instance
        LParsedAnswer.Free;
      end;
  finally
    // free response dictionary instance
    LRespDict.Free;
  end;    
    
  finally
    // free session instance
    LSession.Free;
  end;
end;

function TBardAPI.SaveConversation(const AFilename: string): Boolean;
var
  LSession: TStringList;
  LFilename: string;
begin
  Result := False;
  if aFilename.IsEmpty then Exit;
  LFilename := TPath.ChangeExtension(AFilename, 'txt');

  LSession := TStringList.Create;
  try
    LSession.AddPair('ConversationID', FConversationID);
    LSession.AddPair('ResponseID', FResponseID);
    LSession.AddPair('ChoiceID', FChoiceID);
    LSession.AddPair('ConversationTitle', FConversationTitle);
    LSession.SaveToFile(LFilename);
    Result := TFile.Exists(LFilename);
  finally
    LSession.Free;
  end;
end;

function TBardAPI.LoadConversation(const AFilename: string): Boolean;
var
  LSession: TStringList;
  LFilename: string;
begin
  Result := False;
  if aFilename.IsEmpty then Exit;
  LFilename := TPath.ChangeExtension(AFilename, 'txt');

  LSession := TStringList.Create;
  try
    LSession.LoadFromFile(LFilename);
    ClearConversation;
    SetConversation(
      LSession.Values['ConversationID'],
      LSession.Values['ResponseID'],
      LSession.Values['ChoiceID'],
      LSession.Values['ConversationTitle']);
    Result := True;
  finally
    LSession.Free;
  end;
end;

procedure TBardAPI.ClearConversation;
begin
  FConversationID := '';
  FResponseID := '';
  FChoiceID := '';
  FConversationTitle := '';
  FLastConversationID := '';
  FSetConversationFlag := False;
end;

procedure TBardAPI.SetConversation(const AConversationID, AResponseID,
  AChoiceID, AConversationTitle: string);
begin
  FConversationID := AConversationID;
  FResponseID := AResponseID;
  FChoiceID := AChoiceID;
  FConversationTitle := AConversationTitle;
  FLastConversationID := AConversationID;
  FSetConversationFlag := True;
end;

end.
