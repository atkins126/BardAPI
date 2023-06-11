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

unit BardAPI.AccessToken;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Winapi.ActiveX,
  Winapi.WebView2, Vcl.Edge, Vcl.StdCtrls;

type
  TBardAcessTokenForm = class(TForm)
    EdgeBrowser: TEdgeBrowser;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EdgeBrowserNavigationCompleted(Sender: TCustomEdgeBrowser;
      IsSuccess: Boolean; WebErrorStatus: TOleEnum);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    function GetAccessToken: string;
  public
    { Public declarations }
    property AcessToken: string read GetAccessToken;
  end;

var
  BardAcessTokenForm: TBardAcessTokenForm;

implementation

uses
  System.IOUtils,
  BardAPI.WebView2_2,
  BardAPI;

{$R *.dfm}

var
  MAcessToken: string = '';
  MCanClose: Boolean = False;

type
  { TCompletedHandler }
  TCompletedHandler = class(TInterfacedObject, ICoreWebView2GetCookiesCompletedHandler)
  private
    function Invoke(CallResult: HResult; const cookieList: ICoreWebView2CookieList): HResult; stdcall;
  end;

{ TCompletedHandler }
function TCompletedHandler.Invoke(CallResult: HResult; const cookieList: ICoreWebView2CookieList): HResult;
var
  i, Count: SYSUINT;
  LCookie: ICoreWebView2Cookie;
  HR: HResult;
  pwc: PWideChar;
begin
  if not Succeeded(CallResult) then
    raise Exception.Create('GetCookies failed');

  HR := cookieList.Get_Count(Count);
  if not Succeeded(HR) then
    raise Exception.Create('cookieList.Get_Count failed');

  if Count > 0 then
  begin
    for i := 0 to Count - 1 do
    begin
      HR := cookieList.GetValueAtIndex(i, LCookie);
      if not Succeeded(HR) then
        raise Exception.Create('cookieList.GetValueAtIndex failed');

      LCookie.Get_name(pwc);
      if pwc = '__Secure-1PSID' then
      begin
        LCookie.Get_value(pwc);
        MAcessToken := pwc;
        MCanClose := True;
        Exit(S_OK);
      end;
    end;
  end;
  Result := S_FALSE;
  MCanClose := True;
end;

function TBardAcessTokenForm.GetAccessToken: string;
begin
  Result := MAcessToken;
end;

procedure TBardAcessTokenForm.EdgeBrowserNavigationCompleted(Sender: TCustomEdgeBrowser; IsSuccess: Boolean; WebErrorStatus: TOleEnum);
var
  CookieMan: ICoreWebView2CookieManager;
  CompletedHandler: ICoreWebView2GetCookiesCompletedHandler;
begin
  //
  (EdgeBrowser.DefaultInterface as ICoreWebView2_2).Get_CookieManager(CookieMan);
  CompletedHandler := TCompletedHandler.Create;
  CookieMan.GetCookies('', CompletedHandler);
end;

procedure TBardAcessTokenForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := MCanClose;
end;

procedure TBardAcessTokenForm.FormCreate(Sender: TObject);
var
  LPath: string;
begin
  //
  ClientWidth := 1280;
  ClientHeight := 800;
  MAcessToken := '';
  MCanClose := False;
  EdgeBrowser.UserDataFolder := TBardAPI.GetDataPath + 'WebView2';
  EdgeBrowser.Navigate(CGoogleBardUrl);
end;

procedure TBardAcessTokenForm.FormDestroy(Sender: TObject);
begin
  //
end;

end.
