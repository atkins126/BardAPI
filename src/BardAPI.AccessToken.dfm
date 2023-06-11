object BardAcessTokenForm: TBardAcessTokenForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Bard Access Token'
  ClientHeight = 778
  ClientWidth = 1024
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -18
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 144
  TextHeight = 25
  object EdgeBrowser: TEdgeBrowser
    Left = 0
    Top = 0
    Width = 1024
    Height = 778
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    TabOrder = 0
    UserDataFolder = '%LOCALAPPDATA%\bds.exe.WebView2'
    OnNavigationCompleted = EdgeBrowserNavigationCompleted
    ExplicitWidth = 1014
    ExplicitHeight = 776
  end
end
