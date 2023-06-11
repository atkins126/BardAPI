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

unit BardAPI.WebView2_2;

interface

uses
  System.SysUtils,
  Winapi.ActiveX,
  Winapi.WebView2;

// Constants for enum COREWEBVIEW2_COOKIE_SAME_SITE_KIND
type
  COREWEBVIEW2_COOKIE_SAME_SITE_KIND = TOleEnum;

const
  COREWEBVIEW2_COOKIE_SAME_SITE_KIND_NONE = $00000000;
  COREWEBVIEW2_COOKIE_SAME_SITE_KIND_LAX = $00000001;
  COREWEBVIEW2_COOKIE_SAME_SITE_KIND_STRICT = $00000002;

const
  IID_ICoreWebView2_2: TGUID = '{9E8F0CF8-E670-4B5E-B2BC-73E061E3184C}';
  IID_ICoreWebView2CookieManager: TGUID = '{177CD9E7-B6F5-451A-94A0-5D7A3A4C4141}';

type
  ICoreWebView2_2 = interface;
  ICoreWebView2WebResourceResponseReceivedEventHandler = interface;
  ICoreWebView2DOMContentLoadedEventHandler = interface;
  ICoreWebView2WebResourceResponseReceivedEventArgs = interface;
  ICoreWebView2DOMContentLoadedEventArgs = interface;
  ICoreWebView2WebResourceResponseView = interface;
  ICoreWebView2WebResourceResponseViewGetContentCompletedHandler = interface;
  ICoreWebView2CookieManager = interface;
  ICoreWebView2Cookie = interface;
  ICoreWebView2GetCookiesCompletedHandler = interface;
  ICoreWebView2CookieList = interface;

// *********************************************************************//
// Interface: ICoreWebView2_2
// Flags:     (0)
// GUID:      {9E8F0CF8-E670-4B5E-B2BC-73E061E3184C}
// *********************************************************************//
  ICoreWebView2_2 = interface(ICoreWebView2)
    ['{9E8F0CF8-E670-4B5E-B2BC-73E061E3184C}']
    function add_WebResourceResponseReceived(const eventHandler: ICoreWebView2WebResourceResponseReceivedEventHandler;
                                             out token: EventRegistrationToken): HResult; stdcall;
    function remove_WebResourceResponseReceived(token: EventRegistrationToken): HResult; stdcall;
    function NavigateWithWebResourceRequest(const Request: ICoreWebView2WebResourceRequest): HResult; stdcall;
    function add_DOMContentLoaded(const eventHandler: ICoreWebView2DOMContentLoadedEventHandler;
                                  out token: EventRegistrationToken): HResult; stdcall;
    function remove_DOMContentLoaded(token: EventRegistrationToken): HResult; stdcall;
    function Get_CookieManager(out CookieManager: ICoreWebView2CookieManager): HResult; stdcall;
    function Get_Environment(out Environment: ICoreWebView2Environment): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2WebResourceResponseReceivedEventHandler
// Flags:     (0)
// GUID:      {7DE9898A-24F5-40C3-A2DE-D4F458E69828}
// *********************************************************************//
  ICoreWebView2WebResourceResponseReceivedEventHandler = interface(IUnknown)
    ['{7DE9898A-24F5-40C3-A2DE-D4F458E69828}']
    function Invoke(const sender: ICoreWebView2;
                    const args: ICoreWebView2WebResourceResponseReceivedEventArgs): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2DOMContentLoadedEventHandler
// Flags:     (0)
// GUID:      {4BAC7E9C-199E-49ED-87ED-249303ACF019}
// *********************************************************************//
  ICoreWebView2DOMContentLoadedEventHandler = interface(IUnknown)
    ['{4BAC7E9C-199E-49ED-87ED-249303ACF019}']
    function Invoke(const sender: ICoreWebView2; const args: ICoreWebView2DOMContentLoadedEventArgs): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2WebResourceResponseReceivedEventArgs
// Flags:     (0)
// GUID:      {D1DB483D-6796-4B8B-80FC-13712BB716F4}
// *********************************************************************//
  ICoreWebView2WebResourceResponseReceivedEventArgs = interface(IUnknown)
    ['{D1DB483D-6796-4B8B-80FC-13712BB716F4}']
    function Get_Request(out Request: ICoreWebView2WebResourceRequest): HResult; stdcall;
    function Get_Response(out Response: ICoreWebView2WebResourceResponseView): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2DOMContentLoadedEventArgs
// Flags:     (0)
// GUID:      {16B1E21A-C503-44F2-84C9-70ABA5031283}
// *********************************************************************//
  ICoreWebView2DOMContentLoadedEventArgs = interface(IUnknown)
    ['{16B1E21A-C503-44F2-84C9-70ABA5031283}']
    function Get_NavigationId(out NavigationId: Largeuint): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2WebResourceResponseView
// Flags:     (0)
// GUID:      {79701053-7759-4162-8F7D-F1B3F084928D}
// *********************************************************************//
  ICoreWebView2WebResourceResponseView = interface(IUnknown)
    ['{79701053-7759-4162-8F7D-F1B3F084928D}']
    function Get_Headers(out Headers: ICoreWebView2HttpResponseHeaders): HResult; stdcall;
    function Get_StatusCode(out StatusCode: SYSINT): HResult; stdcall;
    function Get_ReasonPhrase(out ReasonPhrase: PWideChar): HResult; stdcall;
    function GetContent(const handler: ICoreWebView2WebResourceResponseViewGetContentCompletedHandler): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2WebResourceResponseViewGetContentCompletedHandler
// Flags:     (0)
// GUID:      {875738E1-9FA2-40E3-8B74-2E8972DD6FE7}
// *********************************************************************//
  ICoreWebView2WebResourceResponseViewGetContentCompletedHandler = interface(IUnknown)
    ['{875738E1-9FA2-40E3-8B74-2E8972DD6FE7}']
    function Invoke(errorCode: HResult; const Content: IStream): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2CookieManager
// Flags:     (0)
// GUID:      {177CD9E7-B6F5-451A-94A0-5D7A3A4C4141}
// *********************************************************************//
  ICoreWebView2CookieManager = interface(IUnknown)
    ['{177CD9E7-B6F5-451A-94A0-5D7A3A4C4141}']
    function CreateCookie(name: PWideChar; value: PWideChar; Domain: PWideChar; Path: PWideChar;
                          out cookie: ICoreWebView2Cookie): HResult; stdcall;
    function CopyCookie(const cookieParam: ICoreWebView2Cookie; out cookie: ICoreWebView2Cookie): HResult; stdcall;
    function GetCookies(uri: PWideChar; const handler: ICoreWebView2GetCookiesCompletedHandler): HResult; stdcall;
    function AddOrUpdateCookie(const cookie: ICoreWebView2Cookie): HResult; stdcall;
    function DeleteCookie(const cookie: ICoreWebView2Cookie): HResult; stdcall;
    function DeleteCookies(name: PWideChar; uri: PWideChar): HResult; stdcall;
    function DeleteCookiesWithDomainAndPath(name: PWideChar; Domain: PWideChar; Path: PWideChar): HResult; stdcall;
    function DeleteAllCookies: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2Cookie
// Flags:     (0)
// GUID:      {AD26D6BE-1486-43E6-BF87-A2034006CA21}
// *********************************************************************//
  ICoreWebView2Cookie = interface(IUnknown)
    ['{AD26D6BE-1486-43E6-BF87-A2034006CA21}']
    function Get_name(out name: PWideChar): HResult; stdcall;
    function Get_value(out value: PWideChar): HResult; stdcall;
    function Set_value(value: PWideChar): HResult; stdcall;
    function Get_Domain(out Domain: PWideChar): HResult; stdcall;
    function Get_Path(out Path: PWideChar): HResult; stdcall;
    function Get_Expires(out Expires: Double): HResult; stdcall;
    function Set_Expires(Expires: Double): HResult; stdcall;
    function Get_IsHttpOnly(out IsHttpOnly: Integer): HResult; stdcall;
    function Set_IsHttpOnly(IsHttpOnly: Integer): HResult; stdcall;
    function Get_SameSite(out SameSite: COREWEBVIEW2_COOKIE_SAME_SITE_KIND): HResult; stdcall;
    function Set_SameSite(SameSite: COREWEBVIEW2_COOKIE_SAME_SITE_KIND): HResult; stdcall;
    function Get_IsSecure(out IsSecure: Integer): HResult; stdcall;
    function Set_IsSecure(IsSecure: Integer): HResult; stdcall;
    function Get_IsSession(out IsSession: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2GetCookiesCompletedHandler
// Flags:     (0)
// GUID:      {5A4F5069-5C15-47C3-8646-F4DE1C116670}
// *********************************************************************//
  ICoreWebView2GetCookiesCompletedHandler = interface(IUnknown)
    ['{5A4F5069-5C15-47C3-8646-F4DE1C116670}']
    function Invoke(result: HResult; const cookieList: ICoreWebView2CookieList): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2CookieList
// Flags:     (0)
// GUID:      {F7F6F714-5D2A-43C6-9503-346ECE02D186}
// *********************************************************************//
  ICoreWebView2CookieList = interface(IUnknown)
    ['{F7F6F714-5D2A-43C6-9503-346ECE02D186}']
    function Get_Count(out Count: SYSUINT): HResult; stdcall;
    function GetValueAtIndex(index: SYSUINT; out cookie: ICoreWebView2Cookie): HResult; stdcall;
  end;

implementation

end.
